package com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface AccountingPeriodRepository
        extends JpaRepository<AccountingPeriod, UUID> {

    Optional<AccountingPeriod>
    findByStartDateLessThanEqualAndEndDateGreaterThanEqual(
            LocalDate date1,
            LocalDate date2
    );
    Optional<AccountingPeriod> findByStartDateAndEndDate(
            LocalDate start,
            LocalDate end
    );

    List<AccountingPeriod> findByEndDateBeforeAndClosedFalse(
            LocalDate date
    );
}