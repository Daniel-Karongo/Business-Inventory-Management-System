package com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
import java.util.UUID;

public interface LedgerEntryRepository extends JpaRepository<LedgerEntry, UUID> {
    boolean existsByAccountId(UUID accountId);
    List<LedgerEntry> findByAccountIdOrderByPostedAtAsc(UUID accountId);
    List<LedgerEntry> findByPostedAtBetween(LocalDateTime from, LocalDateTime to);

    @Query("""
   SELECT a.id,
          COALESCE(SUM(
              CASE
                  WHEN a.type IN (:debitNormalTypes) AND l.direction = :debitDirection
                      THEN l.amount
                  WHEN a.type IN (:debitNormalTypes) AND l.direction = :creditDirection
                      THEN -l.amount
                  WHEN a.type IN (:creditNormalTypes) AND l.direction = :creditDirection
                      THEN l.amount
                  WHEN a.type IN (:creditNormalTypes) AND l.direction = :debitDirection
                      THEN -l.amount
                  ELSE 0
              END
          ), 0)
   FROM LedgerEntry l
   JOIN l.account a
   WHERE a.id IN :accountIds
     AND l.postedAt BETWEEN :start AND :end
   GROUP BY a.id
""")
    List<Object[]> netMovementForAccountsBetween(
            @Param("accountIds") Set<UUID> accountIds,
            @Param("start") LocalDateTime start,
            @Param("end") LocalDateTime end,
            @Param("debitNormalTypes") Set<AccountType> debitNormalTypes,
            @Param("creditNormalTypes") Set<AccountType> creditNormalTypes,
            @Param("debitDirection") EntryDirection debitDirection,
            @Param("creditDirection") EntryDirection creditDirection
    );

    @Query("""
       SELECT COALESCE(SUM(
           CASE
               WHEN a.type IN (:debitNormalTypes) AND l.direction = :debitDirection
                   THEN l.amount
    
               WHEN a.type IN (:debitNormalTypes) AND l.direction = :creditDirection
                   THEN -l.amount
    
               WHEN a.type IN (:creditNormalTypes) AND l.direction = :creditDirection
                   THEN l.amount
    
               WHEN a.type IN (:creditNormalTypes) AND l.direction = :debitDirection
                   THEN -l.amount
    
               ELSE 0
           END
       ), 0)
       FROM LedgerEntry l
       JOIN l.account a
       WHERE a.id = :accountId
         AND l.postedAt BETWEEN :start AND :end
    """)
    BigDecimal netMovementForAccount(
            @Param("accountId") UUID accountId,
            @Param("start") LocalDateTime start,
            @Param("end") LocalDateTime end,
            @Param("debitNormalTypes") Set<AccountType> debitNormalTypes,
            @Param("creditNormalTypes") Set<AccountType> creditNormalTypes,
            @Param("debitDirection") EntryDirection debitDirection,
            @Param("creditDirection") EntryDirection creditDirection
    );

    @Query("""
    SELECT 
        FUNCTION('DATE', l.postedAt),
        a.id,
        COALESCE(SUM(
            CASE
                WHEN a.type IN (:debitNormalTypes) AND l.direction = :debitDirection
                    THEN l.amount
                WHEN a.type IN (:debitNormalTypes) AND l.direction = :creditDirection
                    THEN -l.amount
                WHEN a.type IN (:creditNormalTypes) AND l.direction = :creditDirection
                    THEN l.amount
                WHEN a.type IN (:creditNormalTypes) AND l.direction = :debitDirection
                    THEN -l.amount
                ELSE 0
            END
        ), 0)
    FROM LedgerEntry l
    JOIN l.account a
    WHERE a.id IN :accountIds
      AND l.postedAt BETWEEN :start AND :end
    GROUP BY FUNCTION('DATE', l.postedAt), a.id
    ORDER BY FUNCTION('DATE', l.postedAt)
""")
    List<Object[]> netMovementGroupedByDateAndAccount(
            @Param("accountIds") Set<UUID> accountIds,
            @Param("start") LocalDateTime start,
            @Param("end") LocalDateTime end,
            @Param("debitNormalTypes") Set<AccountType> debitNormalTypes,
            @Param("creditNormalTypes") Set<AccountType> creditNormalTypes,
            @Param("debitDirection") EntryDirection debitDirection,
            @Param("creditDirection") EntryDirection creditDirection
    );

    @Query("""
        SELECT COALESCE(SUM(l.amount), 0)
        FROM LedgerEntry l
        WHERE l.account.type = :expenseType
          AND l.direction = :debitDirection
          AND l.postedAt BETWEEN :from AND :to
    """)
    BigDecimal totalExpensesBetween(
            @Param("from") LocalDateTime from,
            @Param("to") LocalDateTime to,
            @Param("expenseType") AccountType expenseType,
            @Param("debitDirection") EntryDirection debitDirection
    );

    @Query("""
    SELECT 
        FUNCTION('DATEDIFF', CURRENT_DATE, j.postedAt),
        SUM(CASE 
            WHEN l.direction = :creditDirection THEN l.amount
            WHEN l.direction = :debitDirection THEN -l.amount
            ELSE 0
        END)
    FROM LedgerEntry l
    JOIN l.journalEntry j
    WHERE l.account.id = :apAccountId
      AND j.posted = true
      AND j.reversed = false
    GROUP BY j.id, j.postedAt
""")
    List<Object[]> apAgingRaw(
            @Param("apAccountId") UUID apAccountId,
            @Param("creditDirection") EntryDirection creditDirection,
            @Param("debitDirection") EntryDirection debitDirection
    );
}