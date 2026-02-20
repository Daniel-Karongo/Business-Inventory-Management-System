package com.IntegrityTechnologies.business_manager.modules.finance.accounting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountingPeriodRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDate;

@Service
@RequiredArgsConstructor
public class PeriodGuardService {

    private final AccountingPeriodRepository repository;

    public void validateOpenPeriod(LocalDate date) {

        repository
                .findByStartDateLessThanEqualAndEndDateGreaterThanEqual(date, date)
                .filter(AccountingPeriod::isClosed)
                .ifPresent(p -> {
                    throw new IllegalStateException(
                            "Operation blocked. Accounting period is closed."
                    );
                });
    }
}