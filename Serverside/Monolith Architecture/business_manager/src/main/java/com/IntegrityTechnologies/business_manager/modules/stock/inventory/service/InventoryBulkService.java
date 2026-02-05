package com.IntegrityTechnologies.business_manager.modules.stock.inventory.service;

import com.IntegrityTechnologies.business_manager.common.PhoneAndEmailNormalizer;
import com.IntegrityTechnologies.business_manager.common.bulk.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.dto.*;

import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.model.Product;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
@RequiredArgsConstructor
public class InventoryBulkService {

    private final InventoryService inventoryService;
    private final ProductRepository productRepository;
    private final BranchRepository branchRepository;
    private final SupplierRepository supplierRepository;

    public BulkResult<InventoryBulkPreviewResult> bulkReceive(
            BulkRequest<InventoryReceiveBulkRow> request
    ) {

        BulkResult<InventoryBulkPreviewResult> result = new BulkResult<>();
        result.setTotal(request.getItems().size());

        BulkOptions options =
                request.getOptions() != null
                        ? request.getOptions()
                        : new BulkOptions();

        List<InventoryBulkPreviewRow> previewRows = new ArrayList<>();

        long totalUnits = 0;
        var totalCost = java.math.BigDecimal.ZERO;

        for (int i = 0; i < request.getItems().size(); i++) {
            int rowNum = i + 1;
            InventoryReceiveBulkRow row = request.getItems().get(i);

            try {
                validate(row);

                ReceiveStockRequest req = mapToRequest(row);

                InventoryBulkPreviewRow preview =
                        inventoryService.simulateReceive(req);

                previewRows.add(preview);
                totalUnits += preview.getUnitsReceived();
                totalCost = totalCost.add(preview.getTotalCost());

                if (!options.isDryRun()) {
                    inventoryService.receiveStock(req);
                }

                result.addSuccess(null);

            } catch (Exception ex) {
                result.addError(rowNum, ex.getMessage());
            }
        }

        result.getData().add(
                InventoryBulkPreviewResult.builder()
                        .rows(previewRows)
                        .totalUnits(totalUnits)
                        .totalCost(totalCost)
                        .build()
        );

        return result;
    }

    /* =========================
       VALIDATION
       ========================= */

    private void validate(InventoryReceiveBulkRow row) {

        if (row.getProductName() == null || row.getProductName().isBlank())
            throw new IllegalArgumentException("productName is required");

        if (row.getBranchCode() == null || row.getBranchCode().isBlank())
            throw new IllegalArgumentException("branchCode is required");

        if (row.getClassification() == null || row.getClassification().isBlank())
            throw new IllegalArgumentException("classification is required");

        if (row.getSuppliers() == null || row.getSuppliers().isEmpty())
            throw new IllegalArgumentException("At least one supplier is required");
    }

    /* =========================
       MAPPING & RESOLUTION
       ========================= */

    private ReceiveStockRequest mapToRequest(InventoryReceiveBulkRow row) {

    /* =========================
       NORMALIZE INPUT
       ========================= */

        String rawProductName = row.getProductName();
        String rawClassification = row.getClassification();
        String rawBranchCode = row.getBranchCode();

        if (rawProductName == null || rawProductName.isBlank())
            throw new IllegalArgumentException("productName is required");

        if (rawClassification == null || rawClassification.isBlank())
            throw new IllegalArgumentException("classification is required");

        if (rawBranchCode == null || rawBranchCode.isBlank())
            throw new IllegalArgumentException("branchCode is required");

        String productName = PhoneAndEmailNormalizer.toTitleCase(rawProductName.trim());
        String classification = rawClassification.trim().toUpperCase();
        String branchCode = rawBranchCode.trim().toUpperCase();

        String reference =
                row.getReference() != null
                        ? row.getReference().trim()
                        : null;

        String note =
                row.getNote() != null
                        ? row.getNote().trim()
                        : null;

    /* =========================
       RESOLVE PRODUCT
       ========================= */

        Product product = productRepository
                .findByNameIgnoreCase(productName)
                .orElseThrow(() ->
                        new IllegalArgumentException(
                                "Product not found: " + productName
                        )
                );

    /* =========================
       RESOLVE BRANCH
       ========================= */

        Branch branch = branchRepository
                .findByBranchCode(branchCode)
                .orElseThrow(() ->
                        new IllegalArgumentException(
                                "Branch not found: " + branchCode
                        )
                );

    /* =========================
       RESOLVE SUPPLIERS
       ========================= */

        List<SupplierUnit> supplierUnits = new ArrayList<>();

        for (SupplierUnitBulkRow su : row.getSuppliers()) {

            if (su.getSupplierName() == null || su.getSupplierName().isBlank())
                throw new IllegalArgumentException("supplierName is required");

            String supplierName = PhoneAndEmailNormalizer.toTitleCase(su.getSupplierName().trim());

            Supplier supplier = supplierRepository
                    .findByNameIgnoreCase(supplierName)
                    .orElseThrow(() ->
                            new IllegalArgumentException(
                                    "Supplier not found: " + supplierName
                            )
                    );

            SupplierUnit unit = new SupplierUnit();
            unit.setSupplierId(supplier.getId());
            unit.setUnitsSupplied(su.getUnitsSupplied());
            unit.setUnitCost(su.getUnitCost());

            supplierUnits.add(unit);
        }

    /* =========================
       BUILD REQUEST
       ========================= */

        ReceiveStockRequest req = new ReceiveStockRequest();
        req.setProductId(product.getId());
        req.setClassification(classification);
        req.setBranchId(branch.getId());
        req.setSellingPrice(row.getSellingPrice());
        req.setReference(reference);
        req.setNote(note);
        req.setSuppliers(supplierUnits);

        return req;
    }
}