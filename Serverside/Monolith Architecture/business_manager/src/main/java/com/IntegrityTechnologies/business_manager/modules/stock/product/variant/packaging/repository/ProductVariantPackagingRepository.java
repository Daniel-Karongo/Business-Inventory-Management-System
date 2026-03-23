package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.packaging.model.ProductVariantPackaging;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.UUID;

public interface ProductVariantPackagingRepository extends JpaRepository<ProductVariantPackaging, UUID> {

    List<ProductVariantPackaging> findByProductVariantIdAndDeletedFalse(UUID productVariantId);

    ProductVariantPackaging findByProductVariantIdAndIsBaseUnitTrueAndDeletedFalse(UUID productVariantId);

    @Query("""
        SELECT p FROM ProductVariantPackaging p
        WHERE p.productVariant.id IN :variantIds
          AND p.deleted = false
    """)
    List<ProductVariantPackaging> findAllByVariantIds(
            @Param("variantIds") List<UUID> variantIds
    );

    boolean existsByProductVariantIdAndIsBaseUnitFalseAndDeletedFalse(UUID variantId);

    boolean existsByProductVariantIdAndDeletedFalse(UUID variantId);
}