package com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

public interface LedgerEntryRepository extends JpaRepository<LedgerEntry, UUID> {
    boolean existsByAccountId(UUID accountId);
    List<LedgerEntry> findByAccountIdOrderByPostedAtAsc(UUID accountId);
    List<LedgerEntry> findByPostedAtBetween(LocalDateTime from, LocalDateTime to);

    @Query("""
        SELECT COALESCE(SUM(
            CASE 
                WHEN l.direction = 'CREDIT' THEN l.amount
                WHEN l.direction = 'DEBIT' THEN -l.amount
            END
        ), 0)
        FROM LedgerEntry l
        WHERE l.account.id = :accountId
        AND l.postedAt BETWEEN :from AND :to
    """)
    BigDecimal netMovementForAccount(
            @Param("accountId") UUID accountId,
            @Param("from") LocalDateTime from,
            @Param("to") LocalDateTime to
    );

    @Query("""
        SELECT COALESCE(SUM(l.amount), 0)
        FROM LedgerEntry l
        WHERE l.account.type = 'EXPENSE'
        AND l.direction = 'DEBIT'
        AND l.postedAt BETWEEN :from AND :to
    """)
    BigDecimal totalExpensesBetween(
            @Param("from") LocalDateTime from,
            @Param("to") LocalDateTime to
    );

    @Query("""
        SELECT 
            DATEDIFF(CURRENT_DATE, j.postedAt),
            SUM(CASE 
                WHEN l.direction = 'CREDIT' THEN l.amount
                WHEN l.direction = 'DEBIT' THEN -l.amount
            END)
        FROM LedgerEntry l
        JOIN l.journalEntry j
        WHERE l.account.id = :apAccountId
        AND j.posted = true
        AND j.reversed = false
        GROUP BY j.id, j.postedAt
    """)
    List<Object[]> apAgingRaw(@Param("apAccountId") UUID apAccountId);
}