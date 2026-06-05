package com.IntegrityTechnologies.business_manager.modules.finance.tax.base.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.base.dto.TaxStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.base.repository.TaxSystemStateRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.repository.CorporateTaxFilingRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

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
                        .findByTenantIdAndBranchId(
                                tenantId,
                                branchId
                        )
                        .orElse(null);

        LocalDateTime lastFiling =
                filingRepository
                        .findTopByTenantIdAndBranchIdOrderByFiledAtDesc(
                                tenantId,
                                branchId
                        )
                        .map(f -> f.getFiledAt())
                        .orElse(null);

        if (state == null) {

            return new TaxStatus(
                    false,
                    null,
                    null,
                    null,
                    false
            );
        }

        return new TaxStatus(
                state.isVatEnabled(),
                state.getVatRate(),
                state.getCorporateTaxRate(),
                lastFiling,
                state.isLocked()
        );
    }
}