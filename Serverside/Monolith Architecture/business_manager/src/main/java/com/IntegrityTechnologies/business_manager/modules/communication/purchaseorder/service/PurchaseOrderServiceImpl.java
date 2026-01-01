//package com.IntegrityTechnologies.business_manager.modules.communication.purchaseorder.service;
//
//import com.IntegrityTechnologies.business_manager.modules.communication.purchaseorder.model.PurchaseOrder;
//import com.IntegrityTechnologies.business_manager.modules.communication.purchaseorder.dto.PurchaseOrderRepository;
//import com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.AccountingService;
//import com.IntegrityTechnologies.business_manager.modules.stock.inventory.registry.ReceiveStockRequest;
//import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
//import lombok.RequiredArgsConstructor;
//import org.springframework.stereotype.Service;
//import org.springframework.transaction.annotation.Transactional;
//
//import java.math.BigDecimal;
//import java.time.LocalDateTime;
//import java.util.UUID;
//
//@Service
//@RequiredArgsConstructor
//public class PurchaseOrderServiceImpl implements PurchaseOrderService {
//
//    private final PurchaseOrderRepository repo;
//    private final InventoryService inventoryService;
//    private final AccountingService accountingService;
//
//    @Override
//    @Transactional
//    public PurchaseOrder create(PurchaseOrder po) {
//        po.setId(UUID.randomUUID());
//        po.setCreatedAt(LocalDateTime.now());
//        po.setStatus(PurchaseOrder.Status.DRAFT);
//        return repo.save(po);
//    }
//
//    @Override
//    public PurchaseOrder get(UUID id) {
//        return repo.findById(id).orElseThrow(() -> new IllegalArgumentException("PO not found"));
//    }
//
//    @Override
//    public PurchaseOrder update(PurchaseOrder po) {
//        return repo.save(po);
//    }
//
//    @Override
//    public void delete(UUID id) {
//        repo.deleteById(id);
//    }
//
//    /**
//     * Mark PO as received: increase inventory and create accounting entries.
//     */
//    @Transactional
//    public PurchaseOrder receive(UUID purchaseOrderId, UUID receivingBranchId, String performedBy) {
//        PurchaseOrder po = repo.findById(purchaseOrderId).orElseThrow(() -> new IllegalArgumentException("PO not found"));
//        if (po.getStatus() == PurchaseOrder.Status.RECEIVED) return po;
//
//        // Build ReceiveStockRequest for each line (re-using your DTO)
//        for (var line : po.getLines()) {
//            ReceiveStockRequest req = new ReceiveStockRequest();
//            req.setProductId(line.getProductId());
//            req.setBranchId(receivingBranchId);
//            com.IntegrityTechnologies.business_manager.modules.stock.inventory.registry.SupplierUnit su = new com.IntegrityTechnologies.business_manager.modules.stock.inventory.registry.SupplierUnit();
//            // we don't have supplier id per line here; use po.supplier if present
//            if (po.getSupplier() != null) {
//                su.setSupplierId(po.getSupplier().getId());
//            }
//            su.setUnitsSupplied(line.getQuantity().longValue());
//            su.setUnitCost(line.getUnitPrice() != null ? line.getUnitPrice() : BigDecimal.ZERO);
//            req.setSuppliers(java.util.List.of(su));
//            req.setReference("PO:" + po.getId());
//            req.setNote("Received via PO " + po.getPoNumber());
//
//            inventoryService.receiveStock(req);
//        }
//
//        // total amount
//        java.math.BigDecimal total = po.getLines().stream()
//                .map(l -> l.getLineTotal() != null ? l.getLineTotal() : (l.getUnitPrice() == null ? java.math.BigDecimal.ZERO : l.getUnitPrice().multiply(java.math.BigDecimal.valueOf(l.getQuantity()))))
//                .reduce(java.math.BigDecimal.ZERO, java.math.BigDecimal::add);
//
//        // accounting
//        accountingService.recordPurchaseReceipt(po.getId(), po.getSupplier() != null ? po.getSupplier().getId() : null, total, performedBy);
//
//        po.setStatus(PurchaseOrder.Status.RECEIVED);
//        return repo.save(po);
//    }
//}