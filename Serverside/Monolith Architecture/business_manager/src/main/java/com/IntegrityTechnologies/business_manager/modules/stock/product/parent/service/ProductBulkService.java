package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service;

import com.IntegrityTechnologies.business_manager.common.PhoneAndEmailNormalizer;
import com.IntegrityTechnologies.business_manager.common.bulk.BulkOptions;
import com.IntegrityTechnologies.business_manager.common.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.common.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto.SupplierCreateDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto.SupplierDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.service.SupplierService;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.*;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto.ProductVariantCreateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto.ProductVariantDTO;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
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
    private final SupplierService supplierService;

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
    public BulkResult<ProductDTO> bulkFullCreateFrontend(
            ProductBulkFrontendRequestDTO request,
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

    /* ================================
       DRY RUN MODE
       ================================ */

        if (options.isDryRun()) {

            for (int i = 0; i < request.getProducts().size(); i++) {

                int rowNumber = i + 1;
                ProductBulkFrontendRowDTO row =
                        request.getProducts().get(i);

                try {

                    validateFrontendRow(row);
                    validateCategoryAndSuppliers(row, request.getOptions());
                    result.addSuccess(
                            ProductDTO.builder()
                                    .name(row.getName())
                                    .description(row.getDescription())
                                    .categoryName(row.getCategoryName())
                                    .minimumPercentageProfit(row.getMinimumPercentageProfit())
                                    .variants(
                                            row.getVariants() == null
                                                    ? List.of()
                                                    : row.getVariants().stream()
                                                    .map(v -> ProductVariantDTO.builder()
                                                            .classification(v)
                                                            .build())
                                                    .toList()
                                    )
                                    .build()
                    );

                } catch (Exception ex) {
                    result.addError(rowNumber, ex.getMessage());
                }
            }

            return result;
        }

    /* ================================
       REAL INSERT (ALL OR NOTHING)
       ================================ */

        List<ProductDTO> created = new ArrayList<>();

        for (int i = 0; i < request.getProducts().size(); i++) {

            ProductBulkFrontendRowDTO row =
                    request.getProducts().get(i);

            ProductFullCreateDTO internal =
                    mapToInternalDTO(row, i, request, files);

            List<MultipartFile> relevantFiles =
                    filterMultipartFilesForRow(i, request, files);

            ProductDTO saved =
                    productService.fullCreate(internal, relevantFiles);

            created.add(saved);
        }

        result.setSuccess(created.size());
        result.setFailed(0);
        result.setData(created);

        return result;
    }

    private ProductFullCreateDTO mapToInternalDTO(
            ProductBulkFrontendRowDTO row,
            int rowIndex,
            ProductBulkFrontendRequestDTO request,
            List<MultipartFile> files
    ) {

        Category category = categoryRepository
                .findByNameIgnoreCase(row.getCategoryName())
                .orElseGet(() -> {

                    if (!request.getOptions().isCreateMissingCategories()) {
                        throw new IllegalArgumentException(
                                "Unknown category: " + row.getCategoryName()
                        );
                    }

                    Category newCategory = Category.builder()
                            .name(row.getCategoryName())
                            .build();

                    return categoryRepository.save(newCategory);
                });

        List<UUID> supplierIds = new ArrayList<>();

        if (row.getSupplierNames() != null) {

            for (String supplierName : row.getSupplierNames()) {

                Supplier supplier = supplierRepository
                        .findByNameIgnoreCase(supplierName)
                        .orElseGet(() -> {

                            if (!request.getOptions().isCreateMissingSuppliers()) {
                                throw new IllegalArgumentException(
                                        "Unknown supplier: " + supplierName
                                );
                            }

                            return supplierService.createMinimalSupplier(
                                    supplierName,
                                    category
                            );
                        });

                supplierIds.add(supplier.getId());
            }
        }

        ProductCreateDTO product = ProductCreateDTO.builder()
                .name(row.getName())
                .description(row.getDescription())
                .categoryId(category.getId())
                .supplierIds(supplierIds.isEmpty() ? null : supplierIds)
                .minimumPercentageProfit(row.getMinimumPercentageProfit())
                .build();

        List<ProductVariantCreateDTO> variants =
                (row.getVariants() == null || row.getVariants().isEmpty())
                        ? List.of(defaultVariant())
                        : row.getVariants().stream()
                        .map(v -> {
                            ProductVariantCreateDTO dto =
                                    new ProductVariantCreateDTO();
                            dto.setClassification(v);
                            return dto;
                        })
                        .toList();

        List<FileAssignmentDTO> assignments =
                buildFileAssignmentsForRow(rowIndex, request);

        ProductFullCreateDTO full = new ProductFullCreateDTO();
        full.setProduct(product);
        full.setVariants(variants);
        full.setFileAssignments(assignments);

        return full;
    }

    private List<FileAssignmentDTO> buildFileAssignmentsForRow(
            int rowIndex,
            ProductBulkFrontendRequestDTO request
    ) {

        if (request.getFileMetadata() == null
                || request.getFileMetadata().isEmpty()) {
            return List.of();
        }

        List<FileAssignmentDTO> assignments = new ArrayList<>();

        for (ProductBulkFileMetadataDTO meta : request.getFileMetadata()) {

            if (meta.getAssignedRowIndexes() == null
                    || !meta.getAssignedRowIndexes().contains(rowIndex)) {
                continue;
            }

            FileAssignmentDTO dto = new FileAssignmentDTO();
            dto.setFileName(meta.getFileName());
            dto.setAssignToProduct(Boolean.TRUE.equals(meta.getAssignToEntity()));

            if (meta.getRowVariantMap() != null) {

                List<String> variants =
                        meta.getRowVariantMap().get(rowIndex);

                dto.setVariantClassifications(
                        variants == null ? List.of() : variants
                );
            }

            assignments.add(dto);
        }

        return assignments;
    }

    private List<MultipartFile> filterMultipartFilesForRow(
            int rowIndex,
            ProductBulkFrontendRequestDTO request,
            List<MultipartFile> files
    ) {

        if (files == null || files.isEmpty()
                || request.getFileMetadata() == null) {
            return List.of();
        }

        Set<String> fileNamesForRow = new HashSet<>();

        for (ProductBulkFileMetadataDTO meta : request.getFileMetadata()) {

            if (meta.getAssignedRowIndexes() != null
                    && meta.getAssignedRowIndexes().contains(rowIndex)) {

                fileNamesForRow.add(meta.getFileName().trim());
            }
        }

        return files.stream()
                .filter(f -> fileNamesForRow.contains(f.getOriginalFilename()))
                .toList();
    }

    private ProductVariantCreateDTO defaultVariant() {
        ProductVariantCreateDTO dto = new ProductVariantCreateDTO();
        dto.setClassification("STANDARD");
        return dto;
    }

    private void validateCategoryAndSuppliers(
            ProductBulkFrontendRowDTO row,
            BulkOptions options
    ) {

        if (!categoryRepository
                .existsByNameIgnoreCase(row.getCategoryName())
                && !options.isCreateMissingCategories()) {

            throw new IllegalArgumentException(
                    "Unknown category: " + row.getCategoryName()
            );
        }

        if (row.getSupplierNames() != null) {
            for (String supplierName : row.getSupplierNames()) {

                if (!supplierRepository
                        .existsByNameIgnoreCase(supplierName)
                        && !options.isCreateMissingSuppliers()) {

                    throw new IllegalArgumentException(
                            "Unknown supplier: " + supplierName
                    );
                }
            }
        }
    }

    private void validateFrontendRow(
            ProductBulkFrontendRowDTO row
    ) {

        if (row.getName() == null || row.getName().isBlank()) {
            throw new IllegalArgumentException("Product name is required");
        }

        if (row.getCategoryName() == null
                || row.getCategoryName().isBlank()) {
            throw new IllegalArgumentException("Category is required");
        }

        if (productService.existsByName(row.getName())) {
            throw new IllegalArgumentException(
                    "Product already exists: " + row.getName()
            );
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