package com.IntegrityTechnologies.business_manager.modules.stock.inventory.engine;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.base.model.SaleLineBatchSelection;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.AllocationResult;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.BatchReservation;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventoryBatch;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class StockEngine {

    private final StockAllocationService allocationService;
    private final StockReservationService reservationService;
    private final StockConsumptionService consumptionService;
    private final StockTransferService transferService;
    private final StockAdjustmentService adjustmentService;
    private final StockRestoreService restoreService;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    /* =====================================================
       ALLOCATION
    ===================================================== */

    public List<InventoryBatch> allocate(
            UUID variantId,
            UUID branchId,
            long quantity
    ) {
        return allocationService.allocate(variantId, branchId, quantity);
    }

    public long availableQuantity(UUID variantId, UUID branchId) {
        return allocationService.availableQuantity(variantId, branchId);
    }

    public long reservedQuantity(UUID variantId, UUID branchId) {
        return allocationService.reservedQuantity(variantId, branchId);
    }

    public AllocationResult previewAllocation(
            UUID variantId,
            UUID branchId,
            long quantity,
            List<UUID> selectedBatchIds
    ) {
        return allocationService.previewAllocation(
                variantId,
                branchId,
                quantity,
                selectedBatchIds
        );
    }

    /* =====================================================
       RESERVATION
    ===================================================== */

    @Transactional
    public List<BatchReservation> reserve(
            UUID saleId,
            Long saleLineItemId,
            UUID variantId,
            UUID packagingId,
            UUID branchId,
            long baseUnits
    ) {
        return reservationService.reserve(
                saleId,
                saleLineItemId,
                variantId,
                packagingId,
                branchId,
                baseUnits
        );
    }

    @Transactional
    public void reserveWithSelection(
            UUID saleId,
            UUID variantId,
            UUID packagingId,
            UUID branchId,
            long baseUnits,
            long requestedQuantity,
            List<SaleLineBatchSelection> selections
    ) {
        reservationService.reserveWithSelection(
                saleId,
                variantId,
                packagingId,
                branchId,
                baseUnits,
                requestedQuantity,
                selections
        );
    }

    public Map<UUID, Long> reservedPerBatchPublic(UUID variantId, UUID branchId) {
        return allocationService.reservedPerBatch(variantId, branchId);
    }

    @Transactional
    public void release(UUID branchId, UUID saleId) {
        reservationService.release(branchId, saleId);
    }

    /* =====================================================
       CONSUMPTION
    ===================================================== */

    @Transactional
    public void consume(UUID branchId, UUID referenceId) {
        consumptionService.consume(branchId, referenceId);
    }

    @Transactional
    public void consumeAndSync(
            UUID variantId,
            UUID branchId,
            UUID referenceId
    ) {
        consumptionService.consumeAndSync(variantId, branchId, referenceId);
    }

    /* =====================================================
       TRANSFER
    ===================================================== */

    @Transactional
    public BigDecimal transferAndSyncItem(
            UUID variantId,
            UUID fromBranch,
            UUID toBranch,
            long quantity,
            BigDecimal overrideCost
    ) {
        return transferService.transferAndSync(
                variantId,
                fromBranch,
                toBranch,
                quantity,
                overrideCost
        );
    }

    /* =====================================================
       ADJUSTMENT
    ===================================================== */

    @Transactional
    public BigDecimal adjustAndSync(
            UUID variantId,
            UUID branchId,
            long delta,
            BigDecimal unitCost,
            UUID referenceId
    ) {
        return adjustmentService.adjustAndSync(
                variantId,
                branchId,
                delta,
                unitCost,
                referenceId
        );
    }

    /* =====================================================
       TEMP (KEEP FOR NOW)
    ===================================================== */

    @Transactional
    public void restoreAndSync(
            UUID variantId,
            UUID branchId,
            UUID saleId
    ) {
        restoreService.restoreAndSync(variantId, branchId, saleId);
    }
}