package com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.BatchConsumption;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface BatchConsumptionRepository extends JpaRepository<BatchConsumption, UUID> {

    List<BatchConsumption> findBySaleIdAndTenantIdAndBranchId(
            UUID saleId,
            UUID tenantId,
            UUID branchId
    );

    List<BatchConsumption> findByBatchIdAndTenantIdAndBranchId(
            UUID batchId,
            UUID tenantId,
            UUID branchId
    );

    List<BatchConsumption> findBySaleIdAndProductVariantIdAndTenantIdAndBranchId(
            UUID saleId,
            UUID variantId,
            UUID tenantId,
            UUID branchId
    );

    @Query("""
        SELECT 
            bc.batchId,
            SUM(li.lineTotal * (bc.quantity * 1.0 / li.quantity)),
            SUM(bc.quantity * bc.unitCost)
        FROM BatchConsumption bc
        JOIN Sale s ON s.id = bc.saleId
        JOIN s.lineItems li
        WHERE s.status = 'COMPLETED'
          AND bc.tenantId = :tenantId
          AND bc.branchId = :branchId
          AND s.branchId = :branchId
          AND li.productVariantId = bc.productVariantId
        GROUP BY bc.batchId
    """)
    List<Object[]> topBatchProfitRaw(
            UUID tenantId,
            UUID branchId
    );
}