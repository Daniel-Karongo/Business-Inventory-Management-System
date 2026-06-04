package com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.data.domain.*;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

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
    findByTenantIdAndBranchIdOrderByStartDateAsc(
            UUID tenantId,
            UUID branchId
    );

    List<AccountingPeriod>
    findByTenantIdAndBranchId(UUID tenantId, UUID branchId);

    Page<AccountingPeriod>
    findByTenantIdAndBranchId(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    Optional<AccountingPeriod> findByTenantIdAndBranchIdAndId(UUID tenantId, UUID branchId, UUID periodId);

    Optional<AccountingPeriod> findByTenantIdAndId(UUID tenantId, UUID periodId);

    long countByTenantIdAndBranchIdAndClosedTrueAndTaxAccruedFalse(
            UUID tenantId,
            UUID branchId
    );

    @Query("""
            SELECT p
            FROM AccountingPeriod p
            WHERE p.tenantId = :tenantId
              AND p.branchId = :branchId
              AND p.closed = true
              AND p.taxAccrued = false
            ORDER BY p.startDate DESC
            """)
    List<AccountingPeriod> findEligibleCorporateTaxPeriods(
            UUID tenantId,
            UUID branchId
    );
}