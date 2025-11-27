package com.IntegrityTechnologies.business_manager.modules.inventory.controller;

import com.IntegrityTechnologies.business_manager.modules.inventory.dto.AdjustStockRequest;
import com.IntegrityTechnologies.business_manager.modules.inventory.dto.InventoryResponse;
import com.IntegrityTechnologies.business_manager.modules.inventory.dto.ReceiveStockRequest;
import com.IntegrityTechnologies.business_manager.modules.inventory.service.InventoryService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/inventory")
@RequiredArgsConstructor
public class InventoryController {

    private final InventoryService inventoryService;

    @PreAuthorize("hasAnyRole('ADMIN','MANAGER')")
    @PostMapping("/receive")
    public ResponseEntity<InventoryResponse> receiveStock(@RequestBody ReceiveStockRequest req) {
        return ResponseEntity.ok(inventoryService.receiveStock(req));
    }

    @PreAuthorize("hasAnyRole('ADMIN','MANAGER')")
    @PostMapping("/adjust")
    public ResponseEntity<InventoryResponse> adjustStock(@RequestBody AdjustStockRequest req) {
        return ResponseEntity.ok(inventoryService.adjustStock(req));
    }

    @GetMapping("/product/{productId}")
    public ResponseEntity<InventoryResponse> getInventory(@PathVariable UUID productId) {
        InventoryResponse resp = inventoryService.getInventoryForProduct(productId);
        if (resp == null) return ResponseEntity.notFound().build();
        return ResponseEntity.ok(resp);
    }
}