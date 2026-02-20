package com.IntegrityTechnologies.business_manager.modules.finance.accounting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountingPeriodRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.config.TaxProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.enums.BusinessTaxMode;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.service.CorporateTaxService;
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
    private final TaxProperties taxProperties;

    /* ============================================================
       MANUAL CLOSE
    ============================================================ */

    @Transactional
    public void closePeriod(UUID periodId, String user) {

        AccountingPeriod period =
                periodRepository.findById(periodId)
                        .orElseThrow(() -> new IllegalArgumentException("Period not found"));

        executeClose(period, user);
    }

    /* ============================================================
       AUTO CLOSE PREVIOUS MONTH
    ============================================================ */

    @Transactional
    public void autoClosePreviousMonthIfNeeded(String user) {

        LocalDate now = LocalDate.now();

        // Previous month range
        LocalDate start = now.minusMonths(1).withDayOfMonth(1);
        LocalDate end = start.plusMonths(1).minusDays(1);

        periodRepository
                .findByStartDateAndEndDate(start, end)
                .ifPresent(period -> {
                    if (!period.isClosed()) {
                        executeClose(period, user);
                    }
                });
    }

    /* ============================================================
       RECOVERY — CLOSE ALL OVERDUE PERIODS
    ============================================================ */

    @Transactional
    public void closeAllOverduePeriods(String user) {

        LocalDate today = LocalDate.now();

        List<AccountingPeriod> overdue =
                periodRepository.findByEndDateBeforeAndClosedFalse(today);

        for (AccountingPeriod period : overdue) {
            executeClose(period, user);
        }
    }

    /* ============================================================
       INTERNAL CLOSE LOGIC
    ============================================================ */

    private void executeClose(AccountingPeriod period, String user) {

        if (period.isClosed()) {
            return; // idempotent
        }

        if (taxProperties.getBusinessTaxMode() == BusinessTaxMode.CORPORATE) {

            if (!period.isTaxAccrued()) {

                corporateTaxService.accrueCorporateTax(
                        period.getId(),
                        period.getStartDate().atStartOfDay(),
                        period.getEndDate().atTime(23,59,59),
                        user
                );

                period.setTaxAccrued(true);
                period.setTaxAccruedAt(LocalDateTime.now());
            }
        }

        period.setClosed(true);
        periodRepository.save(period);
    }
}