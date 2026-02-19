package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.model.ProductVariantImageAudit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.UUID;

public interface ProductVariantImageAuditRepository
        extends JpaRepository<ProductVariantImageAudit, UUID> {
    @Modifying
    @Query("update ProductVariantImage i set i.deleted = false where i.variant.id in :variantIds")
    void restoreByVariantIds(@Param("variantIds") List<UUID> variantIds);
}