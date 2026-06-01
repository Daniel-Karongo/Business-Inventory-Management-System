package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.controller;

import com.IntegrityTechnologies.business_manager.config.response.PageWrapper;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service.ProductService;
import lombok.RequiredArgsConstructor;
import org.apache.coyote.BadRequestException;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/products/search")
@RequiredArgsConstructor
@TenantUserOnly
public class ProductSearchController {

    private final ProductService productService;

    @GetMapping
    public PageWrapper<ProductDTO> search(
            @RequestParam UUID branchId,
            @RequestParam(required = false) List<Long> categoryIds,
            @RequestParam(required = false) Long categoryId,
            @RequestParam(required = false) String name,
            @RequestParam(required = false) String description,
            @RequestParam(required = false) String keyword,
            @RequestParam(required = false) Double minPrice,
            @RequestParam(required = false) Double maxPrice,
            @RequestParam(required = false) Boolean deleted,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String direction,
            @RequestParam(defaultValue = "false") boolean includeDeleted,
            @RequestParam(required = false) Integer minSuppliers,
            @RequestParam(required = false) Integer maxSuppliers,
            @RequestParam(required = false) UUID supplierId
    ) throws BadRequestException {

        if (categoryId != null) categoryIds = List.of(categoryId);

        return new PageWrapper<>(
                productService.getProductsAdvanced(
                        branchId,
                        categoryIds,
                        name,
                        description,
                        keyword,
                        minPrice != null ? BigDecimal.valueOf(minPrice) : null,
                        maxPrice != null ? BigDecimal.valueOf(maxPrice) : null,
                        deleted,
                        page,
                        size,
                        sortBy,
                        direction,
                        includeDeleted,
                        minSuppliers,
                        maxSuppliers,
                        supplierId
                )
        );
    }
}