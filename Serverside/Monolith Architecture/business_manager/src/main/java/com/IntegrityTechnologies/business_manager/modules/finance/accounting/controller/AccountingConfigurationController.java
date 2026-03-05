package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.config.AccountingProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.AccountingLockService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.GovernanceAuditService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.BranchAccountingSettingsService;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.config.TaxProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.service.TaxSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.util.UUID;

@RestController
@RequestMapping("/api/accounting/config")
@RequiredArgsConstructor
public class AccountingConfigurationController {

    private final TaxProperties taxProperties;
    private final AccountingLockService lockService;
    private final GovernanceAuditService auditService;
    private final BranchAccountingSettingsService branchSettingsService;
    private final TaxSystemStateService taxSystemStateService;

    @GetMapping
    public ConfigResponse get(@RequestParam UUID branchId) {

        SecurityUtils.requireAtLeast(Role.MANAGER);

        var taxState = taxSystemStateService.getOrCreate(branchId);

        return new ConfigResponse(
                branchSettingsService.getMode(branchId).name(),
                taxState.getTaxMode().name(),
                taxState.getCorporateTaxRate(),
                taxState.getVatRate(),
                taxState.isVatEnabled()
        );
    }

    @PostMapping("/revenue-recognition")
    public void updateRevenueRecognition(
            @RequestParam UUID branchId,
            @RequestParam String mode
    ) {

        SecurityUtils.requireAdmin();

        // Prevent changes after journals exist
        lockService.ensureNoJournalsExist(branchId);

        var newMode =
                com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.RevenueRecognitionMode
                        .valueOf(mode.toUpperCase());

        branchSettingsService.updateMode(branchId, newMode);

        auditService.log(
                branchId,
                "UPDATE_REVENUE_RECOGNITION_MODE",
                SecurityUtils.currentUsername(),
                "New mode: " + newMode
        );
    }

    public record ConfigResponse(
            String revenueRecognitionMode,
            String taxMode,
            BigDecimal corporateTaxRate,
            BigDecimal vatRate,
            boolean vatEnabled
    ) {}
}