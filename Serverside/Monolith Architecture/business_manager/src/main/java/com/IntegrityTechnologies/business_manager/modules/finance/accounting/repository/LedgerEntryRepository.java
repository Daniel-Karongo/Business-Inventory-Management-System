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
       JOIN l.journalEntry je
       WHERE a.id IN :accountIds
         AND l.postedAt BETWEEN :start AND :end
         AND je.posted = true
         AND je.reversed = false
         AND (:branchId IS NULL OR je.branch.id = :branchId)
       GROUP BY a.id
    """)
    List<Object[]> netMovementForAccountsBetween(
            @Param("accountIds") Set<UUID> accountIds,
            @Param("start") LocalDateTime start,
            @Param("end") LocalDateTime end,
            @Param("branchId") UUID branchId,
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

    @Query("""
    SELECT a.type,
           COALESCE(SUM(
               CASE
                   WHEN a.type IN :debitNormal
                        AND le.direction = :debit
                        THEN le.amount

                   WHEN a.type IN :debitNormal
                        AND le.direction = :credit
                        THEN -le.amount

                   WHEN a.type IN :creditNormal
                        AND le.direction = :credit
                        THEN le.amount

                   WHEN a.type IN :creditNormal
                        AND le.direction = :debit
                        THEN -le.amount

                   ELSE 0
               END
           ), 0)
    FROM LedgerEntry le
    JOIN le.account a
    JOIN le.journalEntry je
    WHERE je.posted = true
      AND je.reversed = false
      AND a.type IN :incomeExpenseTypes
      AND le.postedAt BETWEEN :start AND :end
      AND (:branchId IS NULL OR je.branch.id = :branchId)
    GROUP BY a.type
""")
    List<Object[]> netMovementByAccountType(
            @Param("start") LocalDateTime start,
            @Param("end") LocalDateTime end,
            @Param("branchId") UUID branchId,
            @Param("debitNormal") Set<AccountType> debitNormal,
            @Param("creditNormal") Set<AccountType> creditNormal,
            @Param("incomeExpenseTypes") Set<AccountType> incomeExpenseTypes,
            @Param("debit") EntryDirection debit,
            @Param("credit") EntryDirection credit
    );

    @Query("""
        SELECT a.code,
               a.name,
               SUM(
                   CASE
                       WHEN le.postedAt < :start
                            AND a.type IN :debitNormal
                            AND le.direction = :debit
                            THEN le.amount
                       WHEN le.postedAt < :start
                            AND a.type IN :debitNormal
                            AND le.direction = :credit
                            THEN -le.amount
                       WHEN le.postedAt < :start
                            AND a.type IN :creditNormal
                            AND le.direction = :credit
                            THEN le.amount
                       WHEN le.postedAt < :start
                            AND a.type IN :creditNormal
                            AND le.direction = :debit
                            THEN -le.amount
                       ELSE 0
                   END
               ) as opening,
    
               SUM(
                   CASE
                       WHEN le.postedAt BETWEEN :start AND :end
                            AND le.direction = :debit
                            THEN le.amount
                       ELSE 0
                   END
               ) as periodDebit,
    
               SUM(
                   CASE
                       WHEN le.postedAt BETWEEN :start AND :end
                            AND le.direction = :credit
                            THEN le.amount
                       ELSE 0
                   END
               ) as periodCredit
    
        FROM LedgerEntry le
        JOIN le.account a
        JOIN le.journalEntry je
        WHERE je.posted = true
          AND je.reversed = false
          AND (:branchId IS NULL OR je.branch.id = :branchId)
        GROUP BY a.code, a.name
        ORDER BY a.code
    """)
    List<Object[]> enterpriseTrialBalance(
            @Param("start") LocalDateTime start,
            @Param("end") LocalDateTime end,
            @Param("branchId") UUID branchId,
            @Param("debitNormal") Set<AccountType> debitNormal,
            @Param("creditNormal") Set<AccountType> creditNormal,
            @Param("debit") EntryDirection debit,
            @Param("credit") EntryDirection credit
    );

    @Query("""
        SELECT je.branch.name,
               a.code,
               a.name,
               SUM(
                    CASE WHEN le.postedAt < :start THEN
                        CASE
                            WHEN a.type IN :debitNormal AND le.direction = :debit THEN le.amount
                            WHEN a.type IN :debitNormal AND le.direction = :credit THEN -le.amount
                            WHEN a.type IN :creditNormal AND le.direction = :credit THEN le.amount
                            WHEN a.type IN :creditNormal AND le.direction = :debit THEN -le.amount
                        END
                    ELSE 0 END
               ),
               SUM(CASE WHEN le.postedAt BETWEEN :start AND :end AND le.direction = :debit THEN le.amount ELSE 0 END),
               SUM(CASE WHEN le.postedAt BETWEEN :start AND :end AND le.direction = :credit THEN le.amount ELSE 0 END)
        FROM LedgerEntry le
        JOIN le.account a
        JOIN le.journalEntry je
        WHERE je.posted = true
          AND je.reversed = false
          AND le.postedAt <= :end
        GROUP BY je.branch.name, a.code, a.name
        ORDER BY je.branch.name, a.code
    """)
    List<Object[]> enterpriseTrialBalanceMultiBranch(
            LocalDateTime start,
            LocalDateTime end,
            Set<AccountType> debitNormal,
            Set<AccountType> creditNormal,
            EntryDirection debit,
            EntryDirection credit
    );

    @Query("""
            SELECT le
            FROM LedgerEntry le
            JOIN le.journalEntry je
            WHERE le.account.id = :accountId
              AND je.posted = true
              AND je.reversed = false
              AND (:branchId IS NULL OR je.branch.id = :branchId)
              AND le.postedAt <= :end
            ORDER BY le.postedAt ASC, le.id ASC
        """)
    List<LedgerEntry> findLedgerEntriesForGeneralLedger(
            @Param("accountId") UUID accountId,
            @Param("end") LocalDateTime end,
            @Param("branchId") UUID branchId
    );

    @Query("""
    SELECT je.branch.name,
           le.postedAt,
           je.description,
           le.direction,
           le.amount,
           a.type
    FROM LedgerEntry le
    JOIN le.journalEntry je
    JOIN le.account a
    WHERE le.account.id = :accountId
      AND je.posted = true
      AND je.reversed = false
      AND le.postedAt <= :end
    ORDER BY je.branch.name, le.postedAt ASC, le.id ASC
""")
    List<Object[]> enterpriseGeneralLedgerMultiBranch(
            @Param("accountId") UUID accountId,
            @Param("end") LocalDateTime end
    );

    @Query("""
    SELECT a.type,
           a.name,
           COALESCE(SUM(
                CASE
                    WHEN a.type = :incomeType AND le.direction = :credit THEN le.amount
                    WHEN a.type = :expenseType AND le.direction = :debit THEN le.amount
                    ELSE 0
                END
           ), 0)
    FROM LedgerEntry le
    JOIN le.account a
    JOIN le.journalEntry je
    WHERE je.posted = true
      AND je.reversed = false
      AND le.postedAt BETWEEN :start AND :end
      AND (:branchId IS NULL OR je.branch.id = :branchId)
      AND a.type IN :incomeExpenseTypes
    GROUP BY a.type, a.name
""")
    List<Object[]> enterpriseProfitAndLoss(
            @Param("start") LocalDateTime start,
            @Param("end") LocalDateTime end,
            @Param("branchId") UUID branchId,
            @Param("incomeExpenseTypes") Set<AccountType> incomeExpenseTypes,
            @Param("incomeType") AccountType incomeType,
            @Param("expenseType") AccountType expenseType,
            @Param("debit") EntryDirection debit,
            @Param("credit") EntryDirection credit
    );

    @Query("""
    SELECT je.branch.name,
           a.type,
           a.name,
           COALESCE(SUM(
                CASE
                    WHEN a.type = :incomeType AND le.direction = :credit THEN le.amount
                    WHEN a.type = :expenseType AND le.direction = :debit THEN le.amount
                    ELSE 0
                END
           ), 0)
    FROM LedgerEntry le
    JOIN le.account a
    JOIN le.journalEntry je
    WHERE je.posted = true
      AND je.reversed = false
      AND le.postedAt BETWEEN :start AND :end
      AND a.type IN :incomeExpenseTypes
    GROUP BY je.branch.name, a.type, a.name
    ORDER BY je.branch.name, a.type, a.name
""")
    List<Object[]> enterpriseProfitAndLossMultiBranch(
            @Param("start") LocalDateTime start,
            @Param("end") LocalDateTime end,
            @Param("incomeExpenseTypes") Set<AccountType> incomeExpenseTypes,
            @Param("incomeType") AccountType incomeType,
            @Param("expenseType") AccountType expenseType
    );

    @Query("""
        SELECT a.type,
               a.code,
               a.name,
               COALESCE(SUM(
                   CASE
                       WHEN a.type IN :debitNormal
                            AND le.direction = :debit
                            THEN le.amount
    
                       WHEN a.type IN :debitNormal
                            AND le.direction = :credit
                            THEN -le.amount
    
                       WHEN a.type IN :creditNormal
                            AND le.direction = :credit
                            THEN le.amount
    
                       WHEN a.type IN :creditNormal
                            AND le.direction = :debit
                            THEN -le.amount
    
                       ELSE 0
                   END
               ), 0)
        FROM LedgerEntry le
        JOIN le.account a
        JOIN le.journalEntry je
        WHERE je.posted = true
          AND je.reversed = false
          AND le.postedAt <= :asAt
          AND a.type IN :balanceSheetTypes
          AND (:branchId IS NULL OR je.branch.id = :branchId)
        GROUP BY a.type, a.code, a.name
        HAVING COALESCE(SUM(le.amount),0) <> 0
        ORDER BY a.type, a.code
    """)
    List<Object[]> enterpriseBalanceSheet(
            @Param("asAt") LocalDateTime asAt,
            @Param("branchId") UUID branchId,
            @Param("balanceSheetTypes") Set<AccountType> balanceSheetTypes,
            @Param("debitNormal") Set<AccountType> debitNormal,
            @Param("creditNormal") Set<AccountType> creditNormal,
            @Param("debit") EntryDirection debit,
            @Param("credit") EntryDirection credit
    );

    @Query("""
        SELECT je.branch.name,
               a.type,
               a.code,
               a.name,
               COALESCE(SUM(
                   CASE
                       WHEN a.type IN :debitNormal
                            AND le.direction = :debit
                            THEN le.amount
    
                       WHEN a.type IN :debitNormal
                            AND le.direction = :credit
                            THEN -le.amount
    
                       WHEN a.type IN :creditNormal
                            AND le.direction = :credit
                            THEN le.amount
    
                       WHEN a.type IN :creditNormal
                            AND le.direction = :debit
                            THEN -le.amount
    
                       ELSE 0
                   END
               ), 0)
        FROM LedgerEntry le
        JOIN le.account a
        JOIN le.journalEntry je
        WHERE je.posted = true
          AND je.reversed = false
          AND le.postedAt <= :asAt
          AND a.type IN :balanceSheetTypes
        GROUP BY je.branch.name, a.type, a.code, a.name
        ORDER BY je.branch.name, a.type, a.code
    """)
    List<Object[]> enterpriseBalanceSheetMultiBranch(
            @Param("asAt") LocalDateTime asAt,
            @Param("balanceSheetTypes") Set<AccountType> balanceSheetTypes,
            @Param("debitNormal") Set<AccountType> debitNormal,
            @Param("creditNormal") Set<AccountType> creditNormal,
            @Param("debit") EntryDirection debit,
            @Param("credit") EntryDirection credit
    );

    @Query("""
        SELECT je.branch.name,
               a.code,
               a.name,
               COALESCE(SUM(
                   CASE
                       WHEN l.direction = :debit THEN l.amount
                       WHEN l.direction = :credit THEN -l.amount
                       ELSE 0
                   END
               ),0)
        FROM LedgerEntry l
        JOIN l.account a
        JOIN l.journalEntry je
        WHERE je.posted = true
          AND je.reversed = false
          AND l.postedAt BETWEEN :start AND :end
          AND a.code IN :cashCodes
          AND (:branchId IS NULL OR je.branch.id = :branchId)
        GROUP BY je.branch.name, a.code, a.name
        ORDER BY je.branch.name, a.code
    """)
    List<Object[]> enterpriseCashFlow(
            @Param("start") LocalDateTime start,
            @Param("end") LocalDateTime end,
            @Param("branchId") UUID branchId,
            @Param("cashCodes") Set<String> cashCodes,
            @Param("debit") EntryDirection debit,
            @Param("credit") EntryDirection credit
    );

    @Query("""
    SELECT je.branch.name,
           a.code,
           a.name,
           COALESCE(SUM(
               CASE
                   WHEN l.direction = :debit THEN l.amount
                   WHEN l.direction = :credit THEN -l.amount
                   ELSE 0
               END
           ),0)
    FROM LedgerEntry l
    JOIN l.account a
    JOIN l.journalEntry je
    WHERE je.posted = true
      AND je.reversed = false
      AND l.postedAt BETWEEN :start AND :end
      AND a.code IN :cashCodes
    GROUP BY je.branch.name, a.code, a.name
    ORDER BY je.branch.name, a.code
""")
    List<Object[]> enterpriseCashFlowMultiBranch(
            @Param("start") LocalDateTime start,
            @Param("end") LocalDateTime end,
            @Param("cashCodes") Set<String> cashCodes,
            @Param("debit") EntryDirection debit,
            @Param("credit") EntryDirection credit
    );

    @Query("""
SELECT je.reference,
       SUM(
           CASE
               WHEN l.postedAt < :start AND l.direction = :debit THEN l.amount
               WHEN l.postedAt < :start AND l.direction = :credit THEN -l.amount
               ELSE 0
           END
       ) as opening,
       SUM(
           CASE
               WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :debit THEN l.amount
               WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :credit THEN -l.amount
               ELSE 0
           END
       ) as movement
FROM LedgerEntry l
JOIN l.journalEntry je
JOIN l.account a
WHERE a.code = :arCode
  AND je.posted = true
  AND je.reversed = false
  AND l.postedAt <= :end
  AND (:branchId IS NULL OR je.branch.id = :branchId)
GROUP BY je.reference
HAVING
(
    SUM(
        CASE
            WHEN l.postedAt < :start AND l.direction = :debit THEN l.amount
            WHEN l.postedAt < :start AND l.direction = :credit THEN -l.amount
            ELSE 0
        END
    )
    +
    SUM(
        CASE
            WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :debit THEN l.amount
            WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :credit THEN -l.amount
            ELSE 0
        END
    )
) > 0
ORDER BY je.reference
""")
    List<Object[]> enterpriseAccountsReceivable(
            @Param("start") LocalDateTime start,
            @Param("end") LocalDateTime end,
            @Param("branchId") UUID branchId,
            @Param("arCode") String arCode
    );

    @Query("""
SELECT je.branch.name,
       je.reference,
       SUM(
           CASE
               WHEN l.postedAt < :start AND l.direction = :debit THEN l.amount
               WHEN l.postedAt < :start AND l.direction = :credit THEN -l.amount
               ELSE 0
           END
       ) as opening,
       SUM(
           CASE
               WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :debit THEN l.amount
               WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :credit THEN -l.amount
               ELSE 0
           END
       ) as movement
FROM LedgerEntry l
JOIN l.journalEntry je
JOIN l.account a
WHERE a.code = :arCode
  AND je.posted = true
  AND je.reversed = false
  AND l.postedAt <= :end
GROUP BY je.branch.name, je.reference
HAVING
(
    SUM(
        CASE
            WHEN l.postedAt < :start AND l.direction = :debit THEN l.amount
            WHEN l.postedAt < :start AND l.direction = :credit THEN -l.amount
            ELSE 0
        END
    )
    +
    SUM(
        CASE
            WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :debit THEN l.amount
            WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :credit THEN -l.amount
            ELSE 0
        END
    )
) > 0
ORDER BY je.branch.name, je.reference
""")
    List<Object[]> enterpriseAccountsReceivableMultiBranch(
            @Param("start") LocalDateTime start,
            @Param("end") LocalDateTime end,
            @Param("arCode") String arCode
    );

    @Query("""
SELECT je.reference,
       SUM(
           CASE
               WHEN l.postedAt < :start AND l.direction = :credit THEN l.amount
               WHEN l.postedAt < :start AND l.direction = :debit THEN -l.amount
               ELSE 0
           END
       ) as opening,
       SUM(
           CASE
               WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :credit THEN l.amount
               WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :debit THEN -l.amount
               ELSE 0
           END
       ) as movement
FROM LedgerEntry l
JOIN l.journalEntry je
JOIN l.account a
WHERE a.code = :apCode
  AND je.posted = true
  AND je.reversed = false
  AND l.postedAt <= :end
  AND (:branchId IS NULL OR je.branch.id = :branchId)
GROUP BY je.reference
HAVING
(
    SUM(
        CASE
            WHEN l.postedAt < :start AND l.direction = :credit THEN l.amount
            WHEN l.postedAt < :start AND l.direction = :debit THEN -l.amount
            ELSE 0
        END
    )
    +
    SUM(
        CASE
            WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :credit THEN l.amount
            WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :debit THEN -l.amount
            ELSE 0
        END
    )
) > 0
ORDER BY je.reference
""")
    List<Object[]> enterpriseAccountsPayable(
            @Param("start") LocalDateTime start,
            @Param("end") LocalDateTime end,
            @Param("branchId") UUID branchId,
            @Param("apCode") String apCode
    );

    @Query("""
SELECT je.branch.name,
       je.reference,
       SUM(
           CASE
               WHEN l.postedAt < :start AND l.direction = :credit THEN l.amount
               WHEN l.postedAt < :start AND l.direction = :debit THEN -l.amount
               ELSE 0
           END
       ) as opening,
       SUM(
           CASE
               WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :credit THEN l.amount
               WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :debit THEN -l.amount
               ELSE 0
           END
       ) as movement
FROM LedgerEntry l
JOIN l.journalEntry je
JOIN l.account a
WHERE a.code = :apCode
  AND je.posted = true
  AND je.reversed = false
  AND l.postedAt <= :end
GROUP BY je.branch.name, je.reference
HAVING
(
    SUM(
        CASE
            WHEN l.postedAt < :start AND l.direction = :credit THEN l.amount
            WHEN l.postedAt < :start AND l.direction = :debit THEN -l.amount
            ELSE 0
        END
    )
    +
    SUM(
        CASE
            WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :credit THEN l.amount
            WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :debit THEN -l.amount
            ELSE 0
        END
    )
) > 0
ORDER BY je.branch.name, je.reference
""")
    List<Object[]> enterpriseAccountsPayableMultiBranch(
            @Param("start") LocalDateTime start,
            @Param("end") LocalDateTime end,
            @Param("apCode") String apCode
    );
}