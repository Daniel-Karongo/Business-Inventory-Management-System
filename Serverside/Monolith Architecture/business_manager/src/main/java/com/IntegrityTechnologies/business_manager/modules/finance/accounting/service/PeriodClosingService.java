package com.IntegrityTechnologies.business_manager.modules.finance.accounting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.control.CloseChecklistService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.AccountingPeriodClosedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.OutboxEventWriter;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.AccountingSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.GovernanceAuditService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountingPeriodRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.enums.BusinessTaxMode;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.service.CorporateTaxService;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.service.TaxSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.IntegrityTechnologies.business_manager.security.BranchTenantGuard;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class PeriodClosingService {

    private final AccountingPeriodRepository periodRepository;
    private final CorporateTaxService corporateTaxService;
    private final AccountingSystemStateService systemStateService;
    private final GovernanceAuditService auditService;
    private final CloseChecklistService checklistService;
    private final TaxSystemStateService taxSystemStateService;
    private final BranchTenantGuard branchTenantGuard;
    private final OutboxEventWriter outboxWriter;

    @Transactional
    public void closePeriod(UUID periodId, String user) {

        UUID tenantId = TenantContext.getTenantId();

        AccountingPeriod period =
                periodRepository.findByTenantIdAndId(tenantId, periodId)
                        .orElseThrow(() -> new IllegalArgumentException("Period not found"));

        branchTenantGuard.validate(period.getBranchId());
        var checklist =
                checklistService.validate(period.getBranchId(), period);

        if (!checklist.canClose()) {

            throw new IllegalStateException(
                    "Close checklist failed: " +
                            "reconciliation=" + checklist.reconciliationValid() +
                            ", integrity=" + checklist.integrityValid() +
                            ", budgets=" + checklist.budgetsApproved() +
                            ", tax=" + checklist.taxAccrued()
            );
        }

        executeClose(period, user);
    }

    @Transactional
    public void autoClosePreviousMonthIfNeeded(String user, UUID branchId) {

        UUID tenantId = TenantContext.getTenantId();

        LocalDate now = LocalDate.now();

        LocalDate start = now.minusMonths(1).withDayOfMonth(1);
        LocalDate end = start.plusMonths(1).minusDays(1);

        branchTenantGuard.validate(branchId);

        periodRepository
                .findByTenantIdAndBranchIdAndStartDateAndEndDate(
                        tenantId,
                        branchId,
                        start,
                        end
                )
                .ifPresent(period -> {
                    if (!period.isClosed()) {
                        closePeriod(period.getId(), user);
                    }
                });
    }

    @Transactional
    public void closeAllOverduePeriods(String user, UUID branchId) {
        branchTenantGuard.validate(branchId);
        LocalDate today = LocalDate.now();

        UUID tenantId = TenantContext.getTenantId();

        List<AccountingPeriod> overdue =
                periodRepository.findByTenantIdAndBranchIdAndEndDateBeforeAndClosedFalse(
                        tenantId,
                        branchId,
                        today
                );

        for (AccountingPeriod period : overdue) {
            closePeriod(period.getId(), user);
        }
    }

    private void executeClose(AccountingPeriod period, String user) {

        UUID tenantId = TenantContext.getTenantId();

        branchTenantGuard.validate(period.getBranchId());

        if (period.isClosed()) return;

        var taxState = taxSystemStateService.getOrCreate(period.getBranchId());

        if (taxState.getTaxMode() == BusinessTaxMode.CORPORATE) {

            if (!period.isTaxAccrued()) {

                corporateTaxService.accrueCorporateTax(
                        period.getId(),
                        period.getBranchId(),
                        period.getStartDate().atStartOfDay(),
                        period.getEndDate().atTime(23, 59, 59),
                        user
                );

                period.setTaxAccrued(true);
                period.setTaxAccruedAt(java.time.LocalDateTime.now());
            }
        }

        period.setClosed(true);
        periodRepository.save(period);

        auditService.log(
                period.getBranchId(),
                "PERIOD_CLOSED",
                user,
                "Period ID: " + period.getId()
        );

        AccountingPeriodClosedEvent event =
                new AccountingPeriodClosedEvent(
                        tenantId,
                        period.getId(),
                        period.getBranchId(),
                        period.getStartDate().atStartOfDay(),
                        period.getEndDate().atTime(23,59,59)
                );

        outboxWriter.write(
                "ACCOUNTING_PERIOD_CLOSED",
                period.getBranchId(),
                event
        );

        systemStateService.lockIfNecessary(period.getBranchId());
    }
}