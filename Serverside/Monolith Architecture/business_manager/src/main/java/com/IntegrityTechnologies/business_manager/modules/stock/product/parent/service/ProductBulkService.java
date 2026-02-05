package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service;

import com.IntegrityTechnologies.business_manager.common.PhoneAndEmailNormalizer;
import com.IntegrityTechnologies.business_manager.common.bulk.BulkOptions;
import com.IntegrityTechnologies.business_manager.common.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.common.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductBulkRow;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductCreateDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.dto.ProductVariantDTO;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

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