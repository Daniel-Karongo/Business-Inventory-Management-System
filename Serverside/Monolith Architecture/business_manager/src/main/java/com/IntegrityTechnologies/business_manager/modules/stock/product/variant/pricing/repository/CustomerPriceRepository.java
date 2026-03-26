package com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.product.variant.pricing.model.CustomerPrice;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

public interface CustomerPriceRepository extends JpaRepository<CustomerPrice, UUID> {

    @Query("""
        SELECT cp FROM CustomerPrice cp
        WHERE cp.productVariant.id = :variantId
          AND cp.packaging.id = :packagingId
          AND cp.tenantId = :tenantId
          AND cp.branchId = :branchId
          AND cp.deleted = false
          AND (
              (cp.customerId IS NOT NULL AND cp.customerId = :customerId)
              OR
              (cp.customerGroupId IS NOT NULL AND cp.customerGroupId = :groupId)
          )
          AND cp.minQuantity <= :quantity
          AND (cp.validFrom IS NULL OR cp.validFrom <= :now)
          AND (cp.validTo IS NULL OR cp.validTo >= :now)
        ORDER BY cp.minQuantity DESC
    """)
    List<CustomerPrice> findBestMatch(
            UUID variantId,
            UUID packagingId,
            UUID tenantId,
            UUID branchId,
            UUID customerId,
            UUID groupId,
            Long quantity,
            LocalDateTime now
    );
}