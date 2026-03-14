package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.AccountingSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.ReconciliationState;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.ReconciliationStateRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountingPeriodRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.JournalEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.security.JournalIntegrityAudit;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.security.JournalIntegrityAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.BranchAccountingSettingsService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.IntegrityTechnologies.business_manager.security.BranchResolver;
import com.IntegrityTechnologies.business_manager.security.BranchTenantGuard;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.time.LocalDate;
import java.util.UUID;

@RestController
@RequestMapping("/api/accounting/system")
@RequiredArgsConstructor
@TenantAdminOnly
public class AccountingSystemStatusController {

    private final AccountingSystemStateService systemStateService;
    private final AccountRepository accountRepository;
    private final JournalEntryRepository journalRepository;
    private final AccountingPeriodRepository periodRepository;
    private final JournalIntegrityAuditRepository auditRepository;
    private final ReconciliationStateRepository reconciliationRepository;
    private final BranchResolver branchResolver;
    private final BranchAccountingSettingsService branchAccountingSettingsService;
    private final BranchTenantGuard branchTenantGuard;

    @GetMapping("/status")
    public SystemStatusResponse status(@RequestParam(required = false) UUID branchId) {
        branchTenantGuard.validate(branchId);
        UUID effectiveBranchId = branchResolver.resolveBranch(branchId);

        var state = systemStateService.getState(effectiveBranchId);

        long totalAccounts = accountRepository.count();
        UUID tenantId = TenantContext.getTenantId();

        long totalJournals =
                journalRepository.countByTenantIdAndBranchId(
                        tenantId,
                        effectiveBranchId
                );

        LocalDate today = LocalDate.now();

        var openPeriod = periodRepository
                .findByTenantIdAndBranchIdAndStartDateLessThanEqualAndEndDateGreaterThanEqual(
                        tenantId,
                        effectiveBranchId,
                        today,
                        today
                )
                .orElse(null);

        var lastClosed = periodRepository.findByTenantIdAndBranchId(
                        tenantId,
                        effectiveBranchId
                )
                .stream()
                .filter(AccountingPeriod::isClosed)
                .max((a, b) -> a.getEndDate().compareTo(b.getEndDate()))
                .orElse(null);

        JournalIntegrityAudit latestAudit =
                auditRepository
                        .findTopByBranchIdOrderByVerifiedAtDesc(effectiveBranchId)
                        .orElse(null);

        ReconciliationState reconciliation =
                reconciliationRepository
                        .findByBranchId(effectiveBranchId)
                        .orElse(null);

        var cutoff = java.time.LocalDateTime.now().minusDays(1);

        boolean reconciliationConfigured = reconciliation != null;

        boolean reconciliationFresh =
                reconciliation != null
                        && reconciliation.getLastRunAt() != null
                        && reconciliation.getLastRunAt().isAfter(cutoff);

        boolean reconciliationConsistent =
                reconciliationFresh
                        && reconciliation.getInconsistenciesDetected() == 0;

        boolean integrityFresh =
                latestAudit != null
                        && latestAudit.getVerifiedAt() != null
                        && latestAudit.getVerifiedAt().isAfter(cutoff);

        boolean integrityValid =
                integrityFresh
                        && latestAudit.isValid();

        return new SystemStatusResponse(
                effectiveBranchId,
                state.getAccountingMode().name(),
                branchAccountingSettingsService.getMode(effectiveBranchId).name(),
                totalAccounts,
                totalJournals,
                openPeriod != null ? openPeriod.getStartDate() : null,
                openPeriod != null ? openPeriod.getEndDate() : null,
                lastClosed != null ? lastClosed.getEndDate() : null,
                reconciliationConfigured,
                reconciliation != null ? reconciliation.getLastRunAt() : null,
                reconciliationConsistent,
                latestAudit != null ? latestAudit.getVerifiedAt() : null,
                integrityValid
        );
    }

    public record SystemStatusResponse(
            UUID branchId,
            String accountingMode,
            String revenueRecognitionMode,
            long totalAccounts,
            long totalJournals,
            LocalDate currentOpenPeriodStart,
            LocalDate currentOpenPeriodEnd,
            LocalDate lastClosedPeriodEnd,
            boolean reconciliationEnabled,
            java.time.LocalDateTime lastReconciliationRun,
            boolean reconciliationConsistent,
            java.time.LocalDateTime lastIntegrityAudit,
            boolean integrityValid
    ) {}
}