package com.IntegrityTechnologies.business_manager.modules.finance.accounting.control;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.ReconciliationState;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.ReconciliationStateRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.security.JournalIntegrityAudit;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.security.JournalIntegrityAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain.Budget;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain.enums.BudgetStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.repository.BudgetRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.service.TaxSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class CloseChecklistService {

    private static final long FRESHNESS_HOURS = 24;

    private final ReconciliationStateRepository reconciliationRepository;
    private final JournalIntegrityAuditRepository auditRepository;
    private final BudgetRepository budgetRepository;
    private final TaxSystemStateService taxStateService;

    public CloseChecklistResult validate(UUID branchId, AccountingPeriod period) {

        boolean reconciliationValid = validateReconciliation(branchId);
        boolean integrityValid = validateIntegrity(branchId);
        boolean budgetsApproved = validateBudgets(branchId, period);
        boolean taxAccrued = validateTax(branchId, period);

        boolean canClose =
                reconciliationValid &&
                        integrityValid &&
                        budgetsApproved &&
                        taxAccrued;

        return new CloseChecklistResult(
                reconciliationValid,
                integrityValid,
                budgetsApproved,
                taxAccrued,
                canClose
        );
    }

    private boolean validateReconciliation(UUID branchId) {

        ReconciliationState state =
                reconciliationRepository
                        .findByBranchId(branchId)
                        .orElse(null);

        if (state == null) return false;
        if (state.getLastRunAt() == null) return false;

        if (state.getLastRunAt()
                .isBefore(java.time.LocalDateTime.now().minusHours(FRESHNESS_HOURS)))
            return false;

        return state.getInconsistenciesDetected() == 0;
    }

    private boolean validateIntegrity(UUID branchId) {

        JournalIntegrityAudit audit =
                auditRepository
                        .findTopByBranchIdOrderByVerifiedAtDesc(branchId)
                        .orElse(null);

        if (audit == null) return false;
        if (audit.getVerifiedAt() == null) return false;

        if (audit.getVerifiedAt()
                .isBefore(java.time.LocalDateTime.now().minusHours(FRESHNESS_HOURS)))
            return false;

        return audit.isValid();
    }

    private boolean validateBudgets(UUID branchId, AccountingPeriod period) {

        int fiscalYear = period.getStartDate().getYear();

        var branchBudgets =
                budgetRepository.findByTenantIdAndBranchIdAndFiscalYear(
                        TenantContext.getTenantId(),
                        branchId,
                        fiscalYear
                );

        var globalBudgets =
                budgetRepository.findByTenantIdAndBranchIdIsNullAndFiscalYear(
                        TenantContext.getTenantId(),
                        fiscalYear
                );

        return branchBudgets.stream().noneMatch(this::isBlockingStatus)
                && globalBudgets.stream().noneMatch(this::isBlockingStatus);
    }

    private boolean isBlockingStatus(Budget b) {
        return b.getStatus() == BudgetStatus.DRAFT
                || b.getStatus() == BudgetStatus.SUBMITTED;
    }

    private boolean validateTax(UUID branchId, AccountingPeriod period) {

        var taxState = taxStateService.getOrCreate(branchId);

        if (taxState.getTaxMode() !=
                com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.enums.BusinessTaxMode.CORPORATE) {
            return true;
        }

        return period.isTaxAccrued();
    }

    public record CloseChecklistResult(
            boolean reconciliationValid,
            boolean integrityValid,
            boolean budgetsApproved,
            boolean taxAccrued,
            boolean canClose
    ) {}
}