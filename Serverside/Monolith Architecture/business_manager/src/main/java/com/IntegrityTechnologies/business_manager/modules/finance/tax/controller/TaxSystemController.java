package com.IntegrityTechnologies.business_manager.modules.finance.tax.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.GovernanceAuditService;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.TaxSystemState;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.TaxSystemStateRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.service.TaxSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantAdminOnly;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@RestController
@RequestMapping("/api/tax/system")
@RequiredArgsConstructor
@TenantAdminOnly
public class TaxSystemController {

    private final TaxSystemStateRepository repository;
    private final GovernanceAuditService auditService;
    private final TaxSystemStateService stateService;

    @PostMapping("/configure")
    public TaxSystemState configure(
            @RequestParam boolean vatEnabled,
            @RequestParam BigDecimal vatRate,
            @RequestParam BigDecimal corporateTaxRate,
            @RequestParam UUID branchId
    ) {

        UUID tenantId = TenantContext.getTenantId();

        TaxSystemState state =
                repository.findByTenantIdAndBranchId(tenantId, branchId)
                        .orElseGet(() ->
                                TaxSystemState.builder()
                                        .tenantId(tenantId)
                                        .branchId(branchId)
                                        .locked(false)
                                        .build()
                        );

        if (state.isLocked()) {
            throw new IllegalStateException("Tax system is locked.");
        }

        state.setVatEnabled(vatEnabled);
        state.setVatRate(vatRate);
        state.setCorporateTaxRate(corporateTaxRate);

        TaxSystemState saved = repository.save(state);

        auditService.log(
                branchId,
                "TAX_CONFIGURATION_CHANGED",
                SecurityUtils.currentUsername(),
                "VAT enabled=" + vatEnabled + ", VAT rate=" + vatRate
        );

        return saved;
    }

    @PostMapping("/lock")
    public void lock(
        @RequestParam UUID branchId
    ) {

        UUID tenantId = TenantContext.getTenantId();

        TaxSystemState state =
                repository.findByTenantIdAndBranchId(tenantId, branchId)
                        .orElseThrow();

        state.setLocked(true);
        state.setLockedAt(LocalDateTime.now());

        repository.save(state);

        auditService.log(
                branchId,
                "TAX_SYSTEM_LOCKED",
                SecurityUtils.currentUsername(),
                "Tax system locked"
        );
    }

    @GetMapping("/configuration")
    public TaxSystemState configuration(
            @RequestParam UUID branchId
    ) {
        return stateService.get(branchId);
    }
}