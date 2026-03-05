package com.IntegrityTechnologies.business_manager.modules.finance.accounting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountingPeriodRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class PeriodGuardService {

    private final AccountingPeriodRepository repository;

    @Transactional(readOnly = true)
    public AccountingPeriod validateOpenPeriod(LocalDate date, UUID branchId) {

        if (branchId == null) {
            throw new IllegalStateException("BranchId required for period validation");
        }

        AccountingPeriod period =
                repository
                        .findByStartDateLessThanEqualAndEndDateGreaterThanEqualAndBranchId(
                                date,
                                date,
                                branchId
                        )
                        .orElseThrow(() ->
                                new IllegalStateException(
                                        "No accounting period defined for branch "
                                                + branchId + " and date " + date
                                )
                        );

        if (period.isClosed()) {
            throw new IllegalStateException(
                    "Accounting period is closed for branch "
                            + branchId + " and date " + date
            );
        }

        return period;
    }
}