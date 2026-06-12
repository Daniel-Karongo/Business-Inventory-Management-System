package com.IntegrityTechnologies.business_manager.modules.stock.product.parent.controller;

import com.IntegrityTechnologies.business_manager.config.response.PageWrapper;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.StockWorkspaceProductDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service.ProductService;
import lombok.RequiredArgsConstructor;
import org.apache.coyote.BadRequestException;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/products/search")
@RequiredArgsConstructor
@TenantUserOnly
public class ProductSearchController {

    private final ProductService productService;

    @GetMapping
    public PageWrapper<StockWorkspaceProductDTO> search(
            @RequestParam UUID branchId,
            @RequestParam(required = false) List<Long> categoryIds,
            @RequestParam(required = false) Long categoryId,
            @RequestParam(required = false) String keyword,
            @RequestParam(required = false) Boolean deleted,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String direction,
            @RequestParam(required = false) UUID supplierId
    ) throws BadRequestException {

        if (categoryId != null) categoryIds = List.of(categoryId);

        return new PageWrapper<>(
                productService.getProductsAdvanced(
                        branchId,
                        categoryIds,
                        keyword,
                        deleted,
                        page,
                        size,
                        sortBy,
                        direction,
                        supplierId
                )
        );
    }
}