package com.IntegrityTechnologies.business_manager.security.device.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformAdminOnly;
import com.IntegrityTechnologies.business_manager.security.device.dto.DeviceApprovalAuditDTO;
import com.IntegrityTechnologies.business_manager.security.device.dto.TrustedDeviceDTO;
import com.IntegrityTechnologies.business_manager.security.device.service.DeviceApprovalAuditService;
import com.IntegrityTechnologies.business_manager.security.device.service.PlatformTrustedDeviceManagementService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/platform/devices")
@RequiredArgsConstructor
@PlatformAdminOnly
public class PlatformTrustedDeviceController {

    private final PlatformTrustedDeviceManagementService service;

    private final DeviceApprovalAuditService auditService;

    @GetMapping
    public ResponseEntity<List<TrustedDeviceDTO>> list() {

        return ResponseEntity.ok(
                service.listPlatformDevices()
        );
    }

    @PatchMapping("/{id}/approve")
    public ResponseEntity<Void> approve(
            @PathVariable UUID id,
            @RequestParam(required = false)
            String reason
    ){
        service.approve(id, reason);

        return ResponseEntity.ok().build();
    }

    @PatchMapping("/{id}/reject")
    public ResponseEntity<Void> reject(
            @PathVariable UUID id,
            @RequestParam(required = false)
            String reason
    ){
        service.reject(id, reason);

        return ResponseEntity.ok().build();
    }

    @PatchMapping("/{id}/rename")
    public ResponseEntity<Void> rename(
            @PathVariable UUID id,
            @RequestParam String name
    ){

        service.rename(
                id,
                name
        );

        return ResponseEntity.ok().build();

    }

    @GetMapping("/{id}/audit")
    public ResponseEntity<List<DeviceApprovalAuditDTO>> audit(
            @PathVariable UUID id
    ){
        return ResponseEntity.ok(
                auditService.history(id)
        );
    }

}