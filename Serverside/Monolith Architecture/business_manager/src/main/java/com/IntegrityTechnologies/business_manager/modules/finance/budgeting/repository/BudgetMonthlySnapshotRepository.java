package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain.BudgetMonthlySnapshot;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface BudgetMonthlySnapshotRepository
        extends JpaRepository<BudgetMonthlySnapshot, UUID> {

    List<BudgetMonthlySnapshot> findByBranchIdAndFiscalYearAndMonthNumber(
            UUID branchId,
            int fiscalYear,
            int monthNumber
    );

    Optional<BudgetMonthlySnapshot> findByBranchIdAndFiscalYearAndMonthNumberAndAccountId(
            UUID branchId,
            int fiscalYear,
            int monthNumber,
            UUID accountId
    );

    List<BudgetMonthlySnapshot> findByFiscalYearAndMonthNumber(
            int fiscalYear,
            int monthNumber
    );

    @Query("""
        SELECT 
            SUM(s.planned),
            SUM(s.actual),
            SUM(s.variance)
        FROM BudgetMonthlySnapshot s
        WHERE s.fiscalYear = :year
          AND s.monthNumber = :month
          AND s.accountId = :accountId
    """)
    Object[] aggregateCorporateVariance(
            @Param("year") int year,
            @Param("month") int month,
            @Param("accountId") UUID accountId
    );

    @Query("""
    SELECT 
        s.branchId,
        SUM(s.planned),
        SUM(s.actual),
        SUM(s.variance)
    FROM BudgetMonthlySnapshot s
    WHERE s.fiscalYear = :year
      AND s.monthNumber = :month
      AND s.accountId = :accountId
      AND s.branchId IS NOT NULL
    GROUP BY s.branchId
""")
    List<Object[]> aggregateByBranch(
            @Param("year") int year,
            @Param("month") int month,
            @Param("accountId") UUID accountId
    );
}