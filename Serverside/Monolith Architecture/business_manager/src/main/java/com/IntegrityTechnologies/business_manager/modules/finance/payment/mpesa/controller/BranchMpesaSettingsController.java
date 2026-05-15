package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.dto.BranchMpesaSettingsDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.dto.BranchMpesaSettingsResponseDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model.BranchMpesaSettings;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.service.BranchMpesaSettingsService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/settings/mpesa")
@RequiredArgsConstructor
@TenantUserOnly
public class BranchMpesaSettingsController {

    private final BranchMpesaSettingsService service;

    @GetMapping("/branch/{branchId}")
    public ResponseEntity<BranchMpesaSettingsResponseDTO> get(
            @PathVariable UUID branchId
    ) {

        return ResponseEntity.ok(
                service.get(branchId)
        );
    }

    @TenantManagerOnly
    @PutMapping("/branch/{branchId}")
    public ResponseEntity<BranchMpesaSettings> update(
            @PathVariable UUID branchId,
            @RequestBody BranchMpesaSettingsDTO dto
    ) {

        return ResponseEntity.ok(
                service.update(
                        branchId,
                        dto
                )
        );
    }
}