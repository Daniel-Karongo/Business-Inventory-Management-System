package com.IntegrityTechnologies.business_manager.security.device.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformUserOrTenantManager;
import com.IntegrityTechnologies.business_manager.security.device.dto.TrustedDeviceDTO;
import com.IntegrityTechnologies.business_manager.security.device.service.TrustedDeviceManagementService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/devices")
@RequiredArgsConstructor
@PlatformUserOrTenantManager
public class TrustedDeviceController {

    private final TrustedDeviceManagementService service;

    @GetMapping("/branch/{branchId}")
    public ResponseEntity<List<TrustedDeviceDTO>> list(@PathVariable UUID branchId) {
        return ResponseEntity.ok(service.list(branchId));
    }

    @PostMapping("/{id}/approve")
    public ResponseEntity<Void> approve(@PathVariable UUID id) {
        service.approve(id);
        return ResponseEntity.ok().build();
    }

    @PostMapping("/{id}/reject")
    public ResponseEntity<Void> reject(@PathVariable UUID id) {
        service.reject(id);
        return ResponseEntity.ok().build();
    }

    @PatchMapping("/{id}/rename")
    public ResponseEntity<Void> rename(
            @PathVariable UUID id,
            @RequestParam String name
    ) {
        service.rename(id, name);
        return ResponseEntity.ok().build();
    }
}