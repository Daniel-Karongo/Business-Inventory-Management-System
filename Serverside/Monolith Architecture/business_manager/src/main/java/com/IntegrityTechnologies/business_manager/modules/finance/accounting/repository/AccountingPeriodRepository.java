package com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface AccountingPeriodRepository
        extends JpaRepository<AccountingPeriod, UUID> {

    Optional<AccountingPeriod>
    findByStartDateLessThanEqualAndEndDateGreaterThanEqualAndBranchId(
            LocalDate date1,
            LocalDate date2,
            UUID branchId
    );

    Optional<AccountingPeriod>
    findByStartDateAndEndDateAndBranchId(
            LocalDate start,
            LocalDate end,
            UUID branchId
    );

    List<AccountingPeriod>
    findByEndDateBeforeAndClosedFalseAndBranchId(
            LocalDate date,
            UUID branchId
    );

    List<AccountingPeriod>
    findByBranchId(UUID branchId);
    Page<AccountingPeriod> findByBranchId(UUID branchId, Pageable pageable);
}