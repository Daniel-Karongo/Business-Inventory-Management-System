package com.IntegrityTechnologies.business_manager.modules.stock.catalog.service;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.SellableProductRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.SellableProductResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.service.SellableProductService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.dto.ProductDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.service.ProductService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.dto.ProductVariantDTO;
import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.base.service.ProductVariantService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class CatalogService {

    private final ProductService productService;
    private final ProductVariantService variantService;
    private final SellableProductService sellableService;

    /* =========================
       PRODUCT LIST (LIGHT)
       ========================= */
    public List<ProductDTO> getProducts(UUID branchId, Boolean deleted) {
        return productService.getAllProducts(branchId, deleted);
    }

    /* =========================
       PRODUCT DETAIL
       ========================= */
    public ProductDTO getProductDetail(UUID productId, UUID branchId) {

        ProductDTO product =
                productService.getProductById(branchId, productId, false);

        List<ProductVariantDTO> variants =
                variantService.getVariantsForProduct(branchId, productId);

        product.setVariants(variants);

        return product;
    }

    /* =========================
       FULL SELLABLE SEARCH
       ========================= */
    public SellableProductResponse searchSellable(SellableProductRequest request) {
        return sellableService.search(request);
    }
}