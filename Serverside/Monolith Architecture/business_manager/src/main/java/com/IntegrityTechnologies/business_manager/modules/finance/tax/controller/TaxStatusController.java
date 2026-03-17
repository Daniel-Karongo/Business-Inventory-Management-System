package com.IntegrityTechnologies.business_manager.modules.finance.tax.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.dto.TaxStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.CorporateTaxFilingRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.TaxSystemStateRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;

import lombok.RequiredArgsConstructor;

import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.UUID;

@RestController
@RequestMapping("/api/tax")
@RequiredArgsConstructor
@TenantManagerOnly
public class TaxStatusController {

    private final TaxSystemStateRepository taxSystemStateRepository;
    private final CorporateTaxFilingRepository filingRepository;
    private final BranchTenantGuard branchTenantGuard;

    @GetMapping("/system/status")
    public TaxStatus status(@RequestParam UUID branchId) {

        branchTenantGuard.validate(branchId);

        UUID tenantId = TenantContext.getTenantId();

        var state =
                taxSystemStateRepository
                        .findByTenantIdAndBranchId(tenantId, branchId)
                        .orElse(null);

        LocalDateTime lastAccrual =
                filingRepository
                        .findTopByTenantIdAndBranchIdOrderByFiledAtDesc(
                                tenantId,
                                branchId
                        )
                        .map(f -> f.getFiledAt())
                        .orElse(null);

        if (state == null) {
            return new TaxStatus(false, null, null, lastAccrual, false);
        }

        return new TaxStatus(
                state.isVatEnabled(),
                state.getVatRate(),
                state.getCorporateTaxRate(),
                lastAccrual,
                state.isLocked()
        );
    }
}