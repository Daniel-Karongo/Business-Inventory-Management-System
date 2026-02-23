package com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.BatchConsumption;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.UUID;

public interface BatchConsumptionRepository extends JpaRepository<BatchConsumption, UUID> {

    List<BatchConsumption> findBySaleId(UUID saleId);

    List<BatchConsumption> findByBatchId(UUID batchId);

    @Query("""
        SELECT 
            bc.batchId,
            SUM(li.lineTotal * (bc.quantity * 1.0 / li.quantity)),
            SUM(bc.quantity * bc.unitCost)
        FROM BatchConsumption bc
        JOIN Sale s ON s.id = bc.saleId
        JOIN s.lineItems li 
             ON li.productVariantId = bc.productVariantId
        WHERE s.status = 'COMPLETED'
        GROUP BY bc.batchId
    """)
    List<Object[]> topBatchProfitRaw();

    List<BatchConsumption> findBySaleIdAndProductVariantId(UUID saleId, UUID variantId);
}