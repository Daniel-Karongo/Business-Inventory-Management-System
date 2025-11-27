package com.IntegrityTechnologies.business_manager.modules.communication.purchaseorder.service;

import com.IntegrityTechnologies.business_manager.modules.communication.purchaseorder.model.PurchaseOrder;

import java.util.UUID;

public interface PurchaseOrderService {
    PurchaseOrder create(PurchaseOrder po);
    PurchaseOrder get(UUID id);
    PurchaseOrder update(PurchaseOrder po);
    void delete(UUID id);
}