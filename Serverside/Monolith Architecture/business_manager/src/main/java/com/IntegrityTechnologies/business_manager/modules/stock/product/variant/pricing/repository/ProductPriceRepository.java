package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.ProductPrice;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface ProductPriceRepository
        extends JpaRepository<ProductPrice, UUID> {

    @Query("""
                SELECT p
                FROM ProductPrice p
                JOIN FETCH p.packaging pkg
                JOIN FETCH p.productVariant pv
                WHERE p.id = :id
            """)
    Optional<ProductPrice> findDetailedById(
            @Param("id") UUID id
    );

    @Query("""
            SELECT p
            FROM ProductPrice p
            JOIN FETCH p.packaging pkg
            JOIN FETCH p.productVariant pv
            WHERE pv.id = :variantId
              AND pkg.id = :packagingId
              AND p.deleted = false
            """)
    List<ProductPrice> findByProductVariantIdAndPackagingIdAndDeletedFalse(
            UUID variantId,
            UUID packagingId
    );

    @Query("""
            SELECT p
            FROM ProductPrice p
            JOIN FETCH p.packaging pkg
            JOIN FETCH p.productVariant pv
            WHERE pv.id = :variantId
              AND pkg.id = :packagingId
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

    @Query("""
            SELECT p
            FROM ProductPrice p
            JOIN FETCH p.packaging pkg
            JOIN FETCH p.productVariant pv
            WHERE pv.id = :variantId
              AND p.tenantId = :tenantId
              AND p.branchId = :branchId
              AND p.deleted = false
            ORDER BY pkg.unitsPerPackaging ASC,
                     p.minQuantity ASC
            """)
    List<ProductPrice>
    findByProductVariantIdAndTenantIdAndBranchIdAndDeletedFalse(
            UUID variantId,
            UUID tenantId,
            UUID branchId
    );
}