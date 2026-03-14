package com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import org.springframework.data.domain.*;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDate;
import java.util.*;

public interface AccountingPeriodRepository
        extends JpaRepository<AccountingPeriod, UUID> {

    Optional<AccountingPeriod>
    findByTenantIdAndBranchIdAndStartDateLessThanEqualAndEndDateGreaterThanEqual(
            UUID tenantId,
            UUID branchId,
            LocalDate date1,
            LocalDate date2
    );

    Optional<AccountingPeriod>
    findByTenantIdAndBranchIdAndStartDateAndEndDate(
            UUID tenantId,
            UUID branchId,
            LocalDate start,
            LocalDate end
    );

    List<AccountingPeriod>
    findByTenantIdAndBranchIdAndEndDateBeforeAndClosedFalse(
            UUID tenantId,
            UUID branchId,
            LocalDate date
    );

    List<AccountingPeriod>
    findByTenantIdAndBranchId(UUID tenantId, UUID branchId);

    Page<AccountingPeriod>
    findByTenantIdAndBranchId(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    Optional<AccountingPeriod> findByTenantIdAndId(UUID tenantId, UUID id);
}