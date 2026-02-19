package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariantImage;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.UUID;

@Repository
public interface ProductVariantImageRepository extends JpaRepository<ProductVariantImage, UUID> {

    List<ProductVariantImage> findByVariant_IdIn(List<UUID> variantIds);

    List<ProductVariantImage> findByVariant_IdAndDeletedFalse(UUID variantId);

    @Modifying
    @Transactional
    @Query("update ProductVariantImage i set i.deleted = true where i.variant.id in :variantIds")
    void softDeleteByVariantIds(@Param("variantIds") List<UUID> variantIds);

    @Modifying
    @Transactional
    @Query("update ProductVariantImage i set i.deleted = false where i.variant.id in :variantIds")
    void restoreByVariantIds(@Param("variantIds") List<UUID> variantIds);

    @Query("""
        select pvi
        from ProductVariantImage pvi
        join fetch pvi.variant v
        where v.id in :variantIds
    """)
    List<ProductVariantImage> findByVariantIdsWithVariant(@Param("variantIds") List<UUID> variantIds);

    @Modifying
    @Query("DELETE FROM ProductVariantImage vi WHERE vi.variant.id IN :variantIds")
    void deleteByVariantIds(@Param("variantIds") List<UUID> variantIds);
}