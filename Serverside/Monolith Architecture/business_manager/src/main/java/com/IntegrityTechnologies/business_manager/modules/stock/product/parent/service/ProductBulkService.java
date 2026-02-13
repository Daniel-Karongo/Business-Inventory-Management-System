package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service;

import com.IntegrityTechnologies.business_manager.common.PhoneAndEmailNormalizer;
import com.IntegrityTechnologies.business_manager.common.bulk.BulkOptions;
import com.IntegrityTechnologies.business_manager.common.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.common.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto.ProductVariantCreateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto.ProductVariantDTO;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.*;

@Service
@RequiredArgsConstructor
public class ProductBulkService {

    private final ProductService productService;
    private final CategoryRepository categoryRepository;
    private final SupplierRepository supplierRepository;

    public BulkResult<ProductDTO> importProducts(
            BulkRequest<ProductBulkRow> request
    ) {

        BulkResult<ProductDTO> result = new BulkResult<>();
        result.setTotal(request.getItems().size());

        BulkOptions options =
                request.getOptions() != null
                        ? request.getOptions()
                        : new BulkOptions();

        Set<String> seenNames = new HashSet<>();

        for (int i = 0; i < request.getItems().size(); i++) {
            int rowNum = i + 1;
            ProductBulkRow row = request.getItems().get(i);

            try {
                validate(row);

                String normalizedName = PhoneAndEmailNormalizer.toTitleCase(row.getName());

                if (!seenNames.add(normalizedName.toLowerCase())) {
                    throw new IllegalArgumentException(
                            "Duplicate product name in import: " + normalizedName
                    );
                }

                if (options.isSkipDuplicates()
                        && productService.existsByName(normalizedName)) {
                    throw new IllegalArgumentException(
                            "Product already exists: " + normalizedName
                    );
                }

                Category category = categoryRepository
                        .findByNameIgnoreCase(row.getCategoryName())
                        .orElseThrow(() ->
                                new IllegalArgumentException(
                                        "Unknown category: " + row.getCategoryName()
                                )
                        );

                Set<UUID> supplierIds = new HashSet<>();
                if (row.getSupplierNames() != null) {
                    for (String supplierName : row.getSupplierNames()) {
                        Supplier supplier = supplierRepository
                                .findByNameIgnoreCase(supplierName)
                                .orElseThrow(() ->
                                        new IllegalArgumentException(
                                                "Unknown supplier: " + supplierName
                                        )
                                );
                        supplierIds.add(supplier.getId());
                    }
                }

                List<String> variants =
                        (row.getVariants() == null || row.getVariants().isEmpty())
                                ? List.of("STANDARD")
                                : row.getVariants();

                /* =========================
                   DRY RUN
                   ========================= */
                if (options.isDryRun()) {

                    List<ProductVariantDTO> previewVariants =
                            variants.stream()
                                    .map(String::trim)
                                    .filter(v -> !v.isBlank())
                                    .map(v ->
                                            ProductVariantDTO.builder()
                                                    .classification(v)
                                                    .build()
                                    )
                                    .toList();

                    result.addSuccess(
                            ProductDTO.builder()
                                    .name(normalizedName)
                                    .description(row.getDescription())
                                    .categoryId(category.getId())
                                    .categoryName(category.getName())
                                    .variants(previewVariants)
                                    .minimumPercentageProfit(
                                            row.getMinimumPercentageProfit()
                                    )
                                    .build()
                    );
                    continue;
                }

                /* =========================
                   REAL INSERT (OWN TX)
                   ========================= */
                ProductCreateDTO dto = ProductCreateDTO.builder()
                        .name(normalizedName)
                        .description(row.getDescription())
                        .barcode(row.getBarcode())
                        .categoryId(category.getId())
                        .supplierIds(
                                supplierIds.isEmpty()
                                        ? null
                                        : new ArrayList<>(supplierIds)
                        )
                        .variants(variants)
                        .minimumPercentageProfit(row.getMinimumPercentageProfit())
                        .build();

                ProductDTO saved = productService.createProduct(dto);
                result.addSuccess(saved);

            } catch (Exception ex) {
                result.addError(rowNum, ex.getMessage());
            }
        }

        return result;
    }

    @Transactional
    public BulkResult<ProductDTO> bulkFullCreate(
            ProductBulkFullCreateDTO request,
            List<MultipartFile> files
    ) throws IOException {

        BulkResult<ProductDTO> result = new BulkResult<>();

        if (request == null || request.getProducts() == null) {
            return result;
        }

        BulkOptions options =
                request.getOptions() != null
                        ? request.getOptions()
                        : new BulkOptions();

        result.setTotal(request.getProducts().size());

    /* ===================================================
       DRY RUN MODE (NO PERSISTENCE)
       =================================================== */

        if (options.isDryRun()) {

            for (int i = 0; i < request.getProducts().size(); i++) {

                int rowNumber = i + 1;
                ProductFullCreateDTO productDto =
                        request.getProducts().get(i);

                try {

                    validateFullCreate(productDto);

                    result.addSuccess(
                            ProductDTO.builder()
                                    .name(productDto.getProduct().getName())
                                    .description(productDto.getProduct().getDescription())
                                    .build()
                    );

                } catch (Exception ex) {
                    result.addError(rowNumber, ex.getMessage());
                }
            }

            return result;
        }

    /* ===================================================
       REAL INSERT MODE (ALL OR NOTHING)
       =================================================== */

        List<ProductDTO> created = new ArrayList<>();

        for (int i = 0; i < request.getProducts().size(); i++) {

            ProductFullCreateDTO productDto =
                    request.getProducts().get(i);

            List<MultipartFile> relevantFiles =
                    filterFilesForProduct(productDto, files);

            ProductDTO saved =
                    productService.fullCreate(productDto, relevantFiles);

            created.add(saved);
        }

    /* ===================================================
       IF WE REACH HERE → ALL SUCCESS
       =================================================== */

        result.setSuccess(created.size());
        result.setFailed(0);
        result.setData(created);

        return result;
    }

    private List<MultipartFile> filterFilesForProduct(
            ProductFullCreateDTO dto,
            List<MultipartFile> files
    ) {

        if (files == null || files.isEmpty()
                || dto.getFileAssignments() == null
                || dto.getFileAssignments().isEmpty()) {
            return List.of();
        }

        List<String> fileNames =
                dto.getFileAssignments()
                        .stream()
                        .map(FileAssignmentDTO::getFileName)
                        .toList();

        return files.stream()
                .filter(f -> fileNames.contains(f.getOriginalFilename()))
                .toList();
    }

    private void validateFullCreate(ProductFullCreateDTO dto) {

        if (dto.getProduct() == null) {
            throw new IllegalArgumentException("Product payload missing");
        }

        ProductCreateDTO product = dto.getProduct();

        if (product.getName() == null || product.getName().isBlank()) {
            throw new IllegalArgumentException("Product name is required");
        }

        if (product.getCategoryId() == null) {
            throw new IllegalArgumentException("Category is required");
        }

        if (productService.existsByName(product.getName())) {
            throw new IllegalArgumentException(
                    "Product already exists: " + product.getName()
            );
        }

        if (dto.getVariants() != null) {

            Set<String> seen = new HashSet<>();

            for (ProductVariantCreateDTO v : dto.getVariants()) {

                if (v.getClassification() == null || v.getClassification().isBlank()) {
                    throw new IllegalArgumentException(
                            "Variant classification required"
                    );
                }

                if (!seen.add(v.getClassification().toLowerCase())) {
                    throw new IllegalArgumentException(
                            "Duplicate variant classification: "
                                    + v.getClassification()
                    );
                }
            }
        }
    }

    /* =========================
       HELPERS
       ========================= */

    private void validate(ProductBulkRow row) {
        if (row.getName() == null || row.getName().isBlank())
            throw new IllegalArgumentException("name is required");

        if (row.getCategoryName() == null || row.getCategoryName().isBlank())
            throw new IllegalArgumentException("categoryName is required");
    }
}