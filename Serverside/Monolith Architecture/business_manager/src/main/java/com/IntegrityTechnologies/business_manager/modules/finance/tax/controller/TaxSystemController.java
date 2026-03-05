package com.IntegrityTechnologies.business_manager.modules.finance.tax.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.GovernanceAuditService;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.TaxSystemState;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.TaxSystemStateRepository;
import com.IntegrityTechnologies.business_manager.security.BranchContext;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@RestController
@RequestMapping("/api/tax/system")
@RequiredArgsConstructor
public class TaxSystemController {

    private final TaxSystemStateRepository repository;
    private final GovernanceAuditService auditService;

    @PostMapping("/configure")
    public TaxSystemState configure(
            @RequestParam boolean vatEnabled,
            @RequestParam BigDecimal vatRate,
            @RequestParam BigDecimal corporateTaxRate
    ) {

        SecurityUtils.requireAdmin();

        UUID branchId = BranchContext.get();

        TaxSystemState state =
                repository.findByBranchId(branchId)
                        .orElse(new TaxSystemState());

        state.setBranchId(branchId);
        state.setVatEnabled(vatEnabled);
        state.setVatRate(vatRate);
        state.setCorporateTaxRate(corporateTaxRate);

        if (state.isLocked()) {
            throw new IllegalStateException("Tax system is locked.");
        }
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
    public void lock() {

        SecurityUtils.requireAdmin();

        UUID branchId = BranchContext.get();

        TaxSystemState state =
                repository.findByBranchId(branchId)
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
}