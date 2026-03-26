package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.ProductPrice;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.UUID;

public interface ProductPriceRepository extends JpaRepository<ProductPrice, UUID> {

    List<ProductPrice> findByProductVariantIdAndPackagingIdAndDeletedFalse(
            UUID variantId,
            UUID packagingId
    );

    @Query("""
        SELECT p FROM ProductPrice p
        WHERE p.productVariant.id = :variantId
          AND p.packaging.id = :packagingId
          AND p.tenantId = :tenantId
          AND p.branchId = :branchId
          AND p.deleted = false
          AND p.minQuantity <= :quantity
        ORDER BY p.minQuantity DESC
    """)
    List<ProductPrice> findApplicablePrices(
            UUID variantId,
            UUID packagingId,
            UUID tenantId,
            UUID branchId,
            Long quantity
    );
}