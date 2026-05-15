package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.controller;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto.BranchSmsSettingsDTO;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto.BranchSmsSettingsResponseDTO;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.BranchSmsSettings;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service.BranchSmsSettingsService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/settings/sms")
@RequiredArgsConstructor
@TenantUserOnly
public class BranchSmsSettingsController {

    private final BranchSmsSettingsService service;

    @GetMapping("/branch/{branchId}")
    public ResponseEntity<BranchSmsSettingsResponseDTO> get(
            @PathVariable UUID branchId
    ) {

        return ResponseEntity.ok(
                service.get(branchId)
        );
    }

    @TenantManagerOnly
    @PutMapping("/branch/{branchId}")
    public ResponseEntity<BranchSmsSettings> update(
            @PathVariable UUID branchId,
            @RequestBody BranchSmsSettingsDTO dto
    ) {

        return ResponseEntity.ok(
                service.update(
                        branchId,
                        dto
                )
        );
    }
}