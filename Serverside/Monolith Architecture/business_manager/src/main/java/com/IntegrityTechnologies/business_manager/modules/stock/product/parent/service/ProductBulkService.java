package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service;

import com.IntegrityTechnologies.business_manager.common.bulk.*;
import com.IntegrityTechnologies.business_manager.modules.stock.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

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

        for (int i = 0; i < request.getItems().size(); i++) {
            int rowNum = i + 1;
            ProductBulkRow row = request.getItems().get(i);

            try {
                validate(row);

                Long categoryId = categoryRepository
                        .findByNameIgnoreCase(row.getCategoryName())
                        .orElseThrow(() ->
                                new IllegalArgumentException(
                                        "Unknown category: " + row.getCategoryName()
                                )
                        ).getId();

                Set<UUID> supplierIds =
                        row.getSupplierNames() == null
                                ? Set.of()
                                : row.getSupplierNames().stream()
                                .map(name ->
                                        supplierRepository.findByNameIgnoreCase(name)
                                                .orElseThrow(() ->
                                                        new IllegalArgumentException(
                                                                "Unknown supplier: " + name
                                                        )
                                                ).getId()
                                )
                                .collect(Collectors.toSet());

                ProductCreateDTO dto = ProductCreateDTO.builder()
                        .name(row.getName())
                        .description(row.getDescription())
                        .barcode(row.getBarcode())
                        .categoryId(categoryId)
                        .supplierIds(
                                supplierIds.isEmpty() ? null : supplierIds.stream().toList()
                        )
                        .variants(row.getVariants())
                        .minimumPercentageProfit(row.getMinimumPercentageProfit())
                        .build();

                if (!request.getOptions().isDryRun()) {
                    ProductDTO saved = productService.createProduct(dto);
                    result.addSuccess(saved);
                }

            } catch (Exception ex) {
                result.addError(rowNum, ex.getMessage());
            }
        }

        return result;
    }

    private void validate(ProductBulkRow row) {
        if (row.getName() == null || row.getName().isBlank())
            throw new IllegalArgumentException("name is required");

        if (row.getCategoryName() == null || row.getCategoryName().isBlank())
            throw new IllegalArgumentException("categoryName is required");
    }
}