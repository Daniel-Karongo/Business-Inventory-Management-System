package com.IntegrityTechnologies.business_manager.modules.stock.inventory.service;

import com.IntegrityTechnologies.business_manager.common.bulk.*;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class InventoryBulkService {

    private final InventoryService inventoryService;

    public BulkResult<Object> bulkReceive(
            BulkRequest<InventoryReceiveBulkRow> request
    ) {

        BulkResult<Object> result = new BulkResult<>();
        result.setTotal(request.getItems().size());

        for (int i = 0; i < request.getItems().size(); i++) {
            int rowNum = i + 1;
            InventoryReceiveBulkRow row = request.getItems().get(i);

            try {
                validate(row);

                ReceiveStockRequest req = new ReceiveStockRequest();
                req.setProductId(row.getProductId());
                req.setProductVariantId(row.getProductVariantId());
                req.setClassification(row.getClassification());
                req.setNewVariantSku(row.getNewVariantSku());
                req.setBranchId(row.getBranchId());
                req.setSellingPrice(row.getSellingPrice());
                req.setReference(row.getReference());
                req.setNote(row.getNote());
                req.setSuppliers(row.getSuppliers());

                if (!request.getOptions().isDryRun()) {
                    Object response =
                            inventoryService.receiveStock(req).getData();
                    result.addSuccess(response);
                }

            } catch (Exception ex) {
                result.addError(rowNum, ex.getMessage());
            }
        }

        return result;
    }

    /* =========================
       VALIDATION
       ========================= */

    private void validate(InventoryReceiveBulkRow row) {
        if (row.getProductId() == null)
            throw new IllegalArgumentException("productId is required");

        if (row.getBranchId() == null)
            throw new IllegalArgumentException("branchId is required");

        boolean existingVariant = row.getProductVariantId() != null;
        boolean newVariant = row.getClassification() != null && !row.getClassification().isBlank();

        if (!existingVariant && !newVariant) {
            throw new IllegalArgumentException(
                    "Either productVariantId or classification must be provided"
            );
        }

        if (row.getSuppliers() == null || row.getSuppliers().isEmpty()) {
            throw new IllegalArgumentException("At least one supplier entry is required");
        }
    }
}