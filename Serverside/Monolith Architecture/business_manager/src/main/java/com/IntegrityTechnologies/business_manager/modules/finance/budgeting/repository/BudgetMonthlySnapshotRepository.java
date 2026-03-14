package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain.BudgetMonthlySnapshot;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface BudgetMonthlySnapshotRepository
        extends JpaRepository<BudgetMonthlySnapshot, UUID> {

    /* ---------------------------------------------------------
       SAFE FETCH
    --------------------------------------------------------- */

    Optional<BudgetMonthlySnapshot>
    findByTenantIdAndBranchIdAndFiscalYearAndMonthNumberAndAccountId(
            UUID tenantId,
            UUID branchId,
            int fiscalYear,
            int monthNumber,
            UUID accountId
    );

    /* ---------------------------------------------------------
       CORPORATE VARIANCE (TENANT ONLY)
    --------------------------------------------------------- */

    @Query("""
        SELECT 
            SUM(s.planned),
            SUM(s.actual),
            SUM(s.variance)
        FROM BudgetMonthlySnapshot s
        WHERE s.tenantId = :tenantId
          AND s.fiscalYear = :year
          AND s.monthNumber = :month
          AND s.accountId = :accountId
    """)
    Object[] aggregateCorporateVariance(
            UUID tenantId,
            int year,
            int month,
            UUID accountId
    );

    /* ---------------------------------------------------------
       BRANCH COMPARISON (TENANT ISOLATED)
    --------------------------------------------------------- */

    @Query("""
        SELECT 
            s.branchId,
            SUM(s.planned),
            SUM(s.actual),
            SUM(s.variance)
        FROM BudgetMonthlySnapshot s
        WHERE s.tenantId = :tenantId
          AND s.fiscalYear = :year
          AND s.monthNumber = :month
          AND s.accountId = :accountId
          AND s.branchId IS NOT NULL
        GROUP BY s.branchId
    """)
    List<Object[]> aggregateByBranch(
            UUID tenantId,
            int year,
            int month,
            UUID accountId
    );

    /* ---------------------------------------------------------
       APPLY ACTUAL DELTA (KAFKA / JOURNAL EVENTS)
    --------------------------------------------------------- */

    @Modifying
    @Query("""
        UPDATE BudgetMonthlySnapshot s
        SET s.actual = s.actual + :delta,
            s.variance = (s.actual + :delta) - s.planned,
            s.updatedAt = :now
        WHERE s.tenantId = :tenantId
          AND s.branchId = :branchId
          AND s.fiscalYear = :year
          AND s.monthNumber = :month
          AND s.accountId = :accountId
    """)
    int applyActualDelta(
            UUID tenantId,
            UUID branchId,
            int year,
            int month,
            UUID accountId,
            BigDecimal delta,
            LocalDateTime now
    );
}