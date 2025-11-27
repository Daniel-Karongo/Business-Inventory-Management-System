package com.IntegrityTechnologies.business_manager.modules.communication.purchaseorder.controller;

import com.IntegrityTechnologies.business_manager.modules.communication.purchaseorder.model.PurchaseOrder;
import com.IntegrityTechnologies.business_manager.modules.communication.purchaseorder.service.PurchaseOrderService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.net.URI;
import java.util.UUID;

@RestController
@RequestMapping("/api/purchase-orders")
@RequiredArgsConstructor
public class PurchaseOrderController {

    private final PurchaseOrderService service;

    @PreAuthorize("hasAnyRole('ADMIN','MANAGER')")
    @PostMapping
    public ResponseEntity<PurchaseOrder> create(@RequestBody PurchaseOrder po) {
        PurchaseOrder created = service.create(po);
        return ResponseEntity.created(URI.create("/api/purchase-orders/" + created.getId())).body(created);
    }

    @PreAuthorize("hasAnyRole('ADMIN','MANAGER')")
    @GetMapping("/{id}")
    public ResponseEntity<PurchaseOrder> get(@PathVariable UUID id) {
        return ResponseEntity.ok(service.get(id));
    }

    @PreAuthorize("hasAnyRole('ADMIN','MANAGER')")
    @PutMapping("/{id}")
    public ResponseEntity<PurchaseOrder> update(@PathVariable UUID id, @RequestBody PurchaseOrder po) {
        po.setId(id);
        return ResponseEntity.ok(service.update(po));
    }

    @PreAuthorize("hasAnyRole('ADMIN','MANAGER')")
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(@PathVariable UUID id) {
        service.delete(id);
        return ResponseEntity.noContent().build();
    }
}