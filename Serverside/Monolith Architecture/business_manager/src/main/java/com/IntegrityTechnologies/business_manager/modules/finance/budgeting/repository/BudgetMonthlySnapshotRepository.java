package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain.BudgetMonthlySnapshot;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface BudgetMonthlySnapshotRepository
        extends JpaRepository<BudgetMonthlySnapshot, UUID> {

    Optional<BudgetMonthlySnapshot> findByBranchIdAndFiscalYearAndMonthNumberAndAccountId(
            UUID branchId,
            int fiscalYear,
            int monthNumber,
            UUID accountId
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

    @Modifying
    @Query("""
        UPDATE BudgetMonthlySnapshot s
        SET s.actual = s.actual + :delta,
            s.variance = (s.actual + :delta) - s.planned,
            s.updatedAt = :now
        WHERE s.branchId = :branchId
          AND s.fiscalYear = :year
          AND s.monthNumber = :month
          AND s.accountId = :accountId
    """)
    int applyActualDelta(
            UUID branchId,
            int year,
            int month,
            UUID accountId,
            BigDecimal delta,
            LocalDateTime now
    );
}