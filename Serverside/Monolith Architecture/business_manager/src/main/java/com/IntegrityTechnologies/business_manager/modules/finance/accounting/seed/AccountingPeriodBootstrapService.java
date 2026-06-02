package com.IntegrityTechnologies.business_manager.modules.finance.accounting.seed;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountingPeriodRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class AccountingPeriodBootstrapService {

    private static final int FUTURE_MONTHS = 24;

    private final AccountingPeriodRepository repository;

    @Transactional
    public void ensurePeriods(
            UUID tenantId,
            UUID branchId
    ) {
        LocalDate currentMonth =
                LocalDate.now().withDayOfMonth(1);

        List<AccountingPeriod> periods =
                repository.findByTenantIdAndBranchIdOrderByStartDateAsc(
                        tenantId,
                        branchId
                );

        if (periods.isEmpty()) {

            for (int i = 0; i <= FUTURE_MONTHS; i++) {

                LocalDate start =
                        currentMonth.plusMonths(i);

                repository.save(
                        AccountingPeriod.builder()
                                .tenantId(tenantId)
                                .branchId(branchId)
                                .startDate(start)
                                .endDate(
                                        start.plusMonths(1).minusDays(1)
                                )
                                .closed(false)
                                .taxAccrued(false)
                                .build()
                );
            }

            return;
        }

        AccountingPeriod last =
                periods.get(periods.size() - 1);

        validateNoOverlaps(periods);

        LocalDate expected =
                last.getStartDate().plusMonths(1);

        LocalDate target =
                currentMonth.plusMonths(FUTURE_MONTHS);

        while (!expected.isAfter(target)) {

            repository.save(
                    AccountingPeriod.builder()
                            .tenantId(tenantId)
                            .branchId(branchId)
                            .startDate(expected)
                            .endDate(
                                    expected.plusMonths(1).minusDays(1)
                            )
                            .closed(false)
                            .taxAccrued(false)
                            .build()
            );

            expected = expected.plusMonths(1);
        }
    }

    private void validateNoOverlaps(
            List<AccountingPeriod> periods
    ) {
        for (int i = 1; i < periods.size(); i++) {

            AccountingPeriod previous =
                    periods.get(i - 1);

            AccountingPeriod current =
                    periods.get(i);

            LocalDate expectedStart =
                    previous.getStartDate().plusMonths(1);

            if (!current.getStartDate().equals(expectedStart)) {

                throw new IllegalStateException(
                        "Accounting periods contain a gap or overlap. "
                                + "Expected "
                                + expectedStart
                                + " but found "
                                + current.getStartDate()
                );
            }

            LocalDate expectedEnd =
                    current.getStartDate()
                            .plusMonths(1)
                            .minusDays(1);

            if (!current.getEndDate().equals(expectedEnd)) {

                throw new IllegalStateException(
                        "Invalid accounting period end date for "
                                + current.getStartDate()
                );
            }
        }
    }
}