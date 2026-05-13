package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.controller;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.dto.BranchEmailSettingsDTO;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model.BranchEmailSettings;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.service.BranchEmailSettingsService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/settings/email")
@RequiredArgsConstructor
@TenantUserOnly
public class BranchEmailSettingsController {

    private final BranchEmailSettingsService service;

    @GetMapping("/branch/{branchId}")
    public ResponseEntity<BranchEmailSettings> get(
            @PathVariable UUID branchId
    ) {

        return ResponseEntity.ok(
                service.get(branchId)
        );
    }

    @TenantManagerOnly
    @PutMapping("/branch/{branchId}")
    public ResponseEntity<BranchEmailSettings> update(
            @PathVariable UUID branchId,
            @RequestBody BranchEmailSettingsDTO dto
    ) {

        return ResponseEntity.ok(
                service.update(
                        branchId,
                        dto
                )
        );
    }
}