package com.IntegrityTechnologies.business_manager.modules.communication.purchaseorder.service;

import com.IntegrityTechnologies.business_manager.modules.communication.purchaseorder.model.PurchaseOrder;
import com.IntegrityTechnologies.business_manager.modules.communication.purchaseorder.repository.PurchaseOrderRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class PurchaseOrderServiceImpl implements PurchaseOrderService {

    private final PurchaseOrderRepository repo;

    @Override
    public PurchaseOrder create(PurchaseOrder po) {
        po.setId(UUID.randomUUID());
        po.setCreatedAt(LocalDateTime.now());
        po.setStatus(PurchaseOrder.Status.DRAFT);
        return repo.save(po);
    }

    @Override
    public PurchaseOrder get(UUID id) {
        return repo.findById(id).orElseThrow(() -> new IllegalArgumentException("PO not found"));
    }

    @Override
    public PurchaseOrder update(PurchaseOrder po) {
        return repo.save(po);
    }

    @Override
    public void delete(UUID id) {
        repo.deleteById(id);
    }
}