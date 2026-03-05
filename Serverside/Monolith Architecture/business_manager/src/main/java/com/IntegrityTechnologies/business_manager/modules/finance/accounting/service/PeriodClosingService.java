package com.IntegrityTechnologies.business_manager.modules.finance.accounting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.control.CloseChecklistService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.AccountingSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.GovernanceAuditService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountingPeriodRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.config.TaxProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.enums.BusinessTaxMode;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.service.CorporateTaxService;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.service.TaxSystemStateService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
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

    @Transactional
    public void closePeriod(UUID periodId, String user) {

        AccountingPeriod period =
                periodRepository.findById(periodId)
                        .orElseThrow(() -> new IllegalArgumentException("Period not found"));

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

        LocalDate now = LocalDate.now();

        LocalDate start = now.minusMonths(1).withDayOfMonth(1);
        LocalDate end = start.plusMonths(1).minusDays(1);

        periodRepository
                .findByStartDateAndEndDateAndBranchId(start, end, branchId)
                .ifPresent(period -> {
                    if (!period.isClosed()) {
                        closePeriod(period.getId(), user);
                    }
                });
    }

    @Transactional
    public void closeAllOverduePeriods(String user, UUID branchId) {

        LocalDate today = LocalDate.now();

        List<AccountingPeriod> overdue =
                periodRepository.findByEndDateBeforeAndClosedFalseAndBranchId(
                        today,
                        branchId
                );

        for (AccountingPeriod period : overdue) {
            closePeriod(period.getId(), user);
        }
    }

    private void executeClose(AccountingPeriod period, String user) {

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

        systemStateService.lockIfNecessary(period.getBranchId());
    }
}