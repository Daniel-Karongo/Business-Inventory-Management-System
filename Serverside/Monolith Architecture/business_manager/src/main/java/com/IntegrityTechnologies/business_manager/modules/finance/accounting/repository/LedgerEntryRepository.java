package com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.projection.LedgerRunningBalanceProjection;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface LedgerEntryRepository extends JpaRepository<LedgerEntry, UUID> {
    boolean existsByTenantIdAndAccount_Id(UUID tenantId, UUID accountId);

    Page<LedgerEntry> findByTenantIdAndAccount_Id(
            UUID tenantId,
            UUID accountId,
            Pageable pageable
    );
    List<LedgerEntry> findByTenantIdAndPostedAtBetween(
            UUID tenantId,
            LocalDateTime from,
            LocalDateTime to
    );
    List<LedgerEntry> findByTenantIdAndAccount_IdOrderByPostedAtAsc(
            UUID tenantId,
            UUID accountId
    );

    List<LedgerEntry> findByTenantIdAndPostedAtBetweenAndJournalEntry_Branch_Id(
            UUID tenantId,
            LocalDateTime from,
            LocalDateTime to,
            UUID branchId
    );

    /*
     ------------------------------------------------------------
     LEDGER RUNNING BALANCE
     ------------------------------------------------------------
     */

    @Query(value = """
        SELECT 
            je.id AS journalId,
            je.reference AS reference,
            le.posted_at AS postedAt,
            le.direction AS direction,
            le.amount AS amount,

            SUM(
                CASE
                    WHEN le.direction = 'DEBIT' THEN le.amount
                    ELSE -le.amount
                END
            ) OVER (
                PARTITION BY le.account_id
                ORDER BY le.posted_at, le.id
                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
            ) AS runningBalance

        FROM ledger_entries le
        JOIN journal_entries je ON je.id = le.journal_entry_id
        WHERE le.account_id = :accountId
          AND je.tenant_id = :tenantId
          AND je.posted = true
          AND je.reversed = false
          AND (:branchId IS NULL OR je.branch_id = :branchId)
        ORDER BY le.posted_at ASC, le.id ASC
    """,
            countQuery = """
        SELECT COUNT(*)
        FROM ledger_entries le
        JOIN journal_entries je ON je.id = le.journal_entry_id
        WHERE le.account_id = :accountId
          AND je.tenant_id = :tenantId
          AND je.posted = true
          AND je.reversed = false
          AND (:branchId IS NULL OR je.branch_id = :branchId)
    """,
            nativeQuery = true)
    Page<LedgerRunningBalanceProjection> findLedgerWithRunningBalance(
            @Param("tenantId") UUID tenantId,
            @Param("accountId") UUID accountId,
            @Param("branchId") UUID branchId,
            Pageable pageable
    );

    /*
     ------------------------------------------------------------
     SINGLE ACCOUNT BALANCE
     ------------------------------------------------------------
     */

    @Query("""
        SELECT COALESCE(
            SUM(
                CASE
                    WHEN le.direction = :debit THEN le.amount
                    ELSE -le.amount
                END
            ),0)
        FROM LedgerEntry le
        WHERE le.account.id = :accountId
          AND le.tenantId = :tenantId
          AND le.branchId = :branchId
    """)
    BigDecimal computeNetBalance(
            UUID tenantId,
            UUID accountId,
            UUID branchId,
            EntryDirection debit
    );

    /*
     ------------------------------------------------------------
     BRANCH BALANCES
     ------------------------------------------------------------
     */

    @Query("""
        SELECT le.account.id,
               COALESCE(SUM(
                    CASE
                        WHEN le.direction = :debit THEN le.amount
                        ELSE -le.amount
                    END
               ),0)
        FROM LedgerEntry le
        JOIN le.journalEntry je
        WHERE je.tenantId = :tenantId
          AND je.branchId = :branchId
          AND je.posted = true
          AND je.reversed = false
        GROUP BY le.account.id
    """)
    List<Object[]> computeBranchBalances(
            UUID tenantId,
            UUID branchId,
            EntryDirection debit
    );

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
   WHERE je.tenantId = :tenantId
     AND a.id IN :accountIds
     AND l.postedAt BETWEEN :start AND :end
     AND je.posted = true
     AND je.reversed = false
     AND (:branchId IS NULL OR je.branchId = :branchId)
   GROUP BY a.id
""")
    List<Object[]> netMovementForAccountsBetween(
            UUID tenantId,
            Set<UUID> accountIds,
            LocalDateTime start,
            LocalDateTime end,
            UUID branchId,
            Set<AccountType> debitNormalTypes,
            Set<AccountType> creditNormalTypes,
            EntryDirection debitDirection,
            EntryDirection creditDirection
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
   JOIN l.journalEntry je
   WHERE je.tenantId = :tenantId
     AND a.id = :accountId
     AND l.postedAt BETWEEN :start AND :end
     AND je.posted = true
     AND je.reversed = false
     AND (:branchId IS NULL OR je.branchId = :branchId)
""")
    BigDecimal netMovementForAccount(
            UUID tenantId,
            UUID accountId,
            LocalDateTime start,
            LocalDateTime end,
            UUID branchId,
            Set<AccountType> debitNormalTypes,
            Set<AccountType> creditNormalTypes,
            EntryDirection debitDirection,
            EntryDirection creditDirection
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
JOIN l.journalEntry je
WHERE je.tenantId = :tenantId
  AND a.id IN :accountIds
  AND l.postedAt BETWEEN :start AND :end
  AND je.posted = true
  AND je.reversed = false
  AND (:branchId IS NULL OR je.branchId = :branchId)
GROUP BY FUNCTION('DATE', l.postedAt), a.id
ORDER BY FUNCTION('DATE', l.postedAt)
""")
    List<Object[]> netMovementGroupedByDateAndAccount(
            UUID tenantId,
            Set<UUID> accountIds,
            LocalDateTime start,
            LocalDateTime end,
            UUID branchId,
            Set<AccountType> debitNormalTypes,
            Set<AccountType> creditNormalTypes,
            EntryDirection debitDirection,
            EntryDirection creditDirection
    );

    @Query("""
SELECT COALESCE(SUM(l.amount), 0)
FROM LedgerEntry l
JOIN l.journalEntry je
WHERE je.tenantId = :tenantId
  AND l.account.type = :expenseType
  AND l.direction = :debitDirection
  AND l.postedAt BETWEEN :from AND :to
  AND je.posted = true
  AND je.reversed = false
  AND (:branchId IS NULL OR je.branchId = :branchId)
""")
    BigDecimal totalExpensesBetween(
            UUID tenantId,
            LocalDateTime from,
            LocalDateTime to,
            UUID branchId,
            AccountType expenseType,
            EntryDirection debitDirection
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
WHERE j.tenantId = :tenantId
  AND l.account.id = :apAccountId
  AND j.posted = true
  AND j.reversed = false
  AND (:branchId IS NULL OR j.branchId = :branchId)
GROUP BY j.id, j.postedAt
""")
    List<Object[]> apAgingRaw(
            UUID tenantId,
            UUID apAccountId,
            UUID branchId,
            EntryDirection creditDirection,
            EntryDirection debitDirection
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
    WHERE je.tenantId = :tenantId
      AND je.posted = true
      AND je.reversed = false
      AND a.type IN :incomeExpenseTypes
      AND le.postedAt BETWEEN :start AND :end
      AND (:branchId IS NULL OR je.branch.id = :branchId)
    GROUP BY a.type
""")
    List<Object[]> netMovementByAccountType(
            @Param("tenantId") UUID tenantId,
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
        SELECT 
            a.id,
            COALESCE(SUM(
                CASE
                    WHEN l.direction = :debit THEN l.amount
                    ELSE 0
                END
            ),0),
            COALESCE(SUM(
                CASE
                    WHEN l.direction = :credit THEN l.amount
                    ELSE 0
                END
            ),0)
        FROM LedgerEntry l
        JOIN l.account a
        JOIN l.journalEntry je
        WHERE je.tenantId = :tenantId
          AND je.posted = true
          AND je.reversed = false
          AND l.postedAt BETWEEN :start AND :end
          AND (:branchId IS NULL OR je.branch.id = :branchId)
        GROUP BY a.id
    """)
    List<Object[]> movementByAccountBetween(
            UUID tenantId,
            LocalDateTime start,
            LocalDateTime end,
            UUID branchId,
            EntryDirection debit,
            EntryDirection credit
    );

    @Query("""
        SELECT 
            a.id,
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
        WHERE je.tenantId = :tenantId
          AND je.posted = true
          AND je.reversed = false
          AND l.postedAt < :before
          AND (:branchId IS NULL OR je.branch.id = :branchId)
        GROUP BY a.id
    """)
    List<Object[]> balanceBeforeDate(
            UUID tenantId,
            LocalDateTime before,
            UUID branchId,
            EntryDirection debit,
            EntryDirection credit
    );
    @Query("""
        SELECT le
        FROM LedgerEntry le
        WHERE le.tenantId = :tenantId
        AND le.account.id = :accountId
        AND le.branchId = :branchId
        AND le.postedAt <= :end
        ORDER BY le.postedAt ASC
    """)
    Page<LedgerEntry> findLedgerEntriesForGeneralLedgerPaged(
            UUID tenantId,
            UUID accountId,
            UUID branchId,
            LocalDateTime end,
            Pageable pageable
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
WHERE je.tenantId = :tenantId
  AND le.account.id = :accountId
  AND je.posted = true
  AND je.reversed = false
  AND le.postedAt <= :end
ORDER BY je.branch.name, le.postedAt ASC, le.id ASC
""")
    List<Object[]> enterpriseGeneralLedgerMultiBranch(
            @Param("tenantId") UUID tenantId,
            @Param("accountId") UUID accountId,
            @Param("end") LocalDateTime end
    );

    /*
     ------------------------------------------------------------
     ENTERPRISE PROFIT & LOSS (MULTI-BRANCH SAFE)
     ------------------------------------------------------------
     */

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
           ),0)
    FROM LedgerEntry le
    JOIN le.account a
    JOIN le.journalEntry je
    WHERE je.tenantId = :tenantId
      AND je.posted = true
      AND je.reversed = false
      AND le.postedAt BETWEEN :start AND :end
      AND a.type IN :incomeExpenseTypes
    GROUP BY je.branch.name, a.type, a.name
    ORDER BY je.branch.name, a.type, a.name
    """)
    List<Object[]> enterpriseProfitAndLossMultiBranch(
            UUID tenantId,
            LocalDateTime start,
            LocalDateTime end,
            Set<AccountType> incomeExpenseTypes,
            AccountType incomeType,
            AccountType expenseType
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
WHERE je.tenantId = :tenantId
  AND je.posted = true
  AND je.reversed = false
  AND l.postedAt BETWEEN :start AND :end
  AND a.code IN :cashCodes
  AND (:branchId IS NULL OR je.branch.id = :branchId)
GROUP BY je.branch.name, a.code, a.name
ORDER BY je.branch.name, a.code
""")
    List<Object[]> enterpriseCashFlow(
            @Param("tenantId") UUID tenantId,
            @Param("start") LocalDateTime start,
            @Param("end") LocalDateTime end,
            @Param("branchId") UUID branchId,
            @Param("cashCodes") Set<String> cashCodes,
            @Param("debit") EntryDirection debit,
            @Param("credit") EntryDirection credit
    );

    /*
     ------------------------------------------------------------
     ENTERPRISE CASH FLOW
     ------------------------------------------------------------
     */

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
        WHERE je.tenantId = :tenantId
          AND je.posted = true
          AND je.reversed = false
          AND l.postedAt BETWEEN :start AND :end
          AND a.code IN :cashCodes
        GROUP BY je.branch.name, a.code, a.name
        ORDER BY je.branch.name, a.code
    """)
    List<Object[]> enterpriseCashFlowMultiBranch(
            UUID tenantId,
            LocalDateTime start,
            LocalDateTime end,
            Set<String> cashCodes,
            EntryDirection debit,
            EntryDirection credit
    );

    @Query("""
SELECT je.reference,
       SUM(
           CASE
               WHEN l.postedAt < :start AND l.direction = :debit THEN l.amount
               WHEN l.postedAt < :start AND l.direction = :credit THEN -l.amount
               ELSE 0
           END) as opening,
       SUM(
           CASE
               WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :debit THEN l.amount
               WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :credit THEN -l.amount
               ELSE 0
           END) as movement
FROM LedgerEntry l
JOIN l.journalEntry je
JOIN l.account a
WHERE je.tenantId = :tenantId
  AND a.code = :arCode
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
        END)
    +
    SUM(
        CASE
            WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :debit THEN l.amount
            WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :credit THEN -l.amount
            ELSE 0
        END)
) > 0
ORDER BY je.reference
""")
    List<Object[]> enterpriseAccountsReceivable(
            @Param("tenantId") UUID tenantId,
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
           END) as opening,
       SUM(
           CASE
               WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :debit THEN l.amount
               WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :credit THEN -l.amount
               ELSE 0
           END) as movement
FROM LedgerEntry l
JOIN l.journalEntry je
JOIN l.account a
WHERE je.tenantId = :tenantId
  AND a.code IN :arCodes
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
        END)
    +
    SUM(
        CASE
            WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :debit THEN l.amount
            WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :credit THEN -l.amount
            ELSE 0
        END)
) > 0
ORDER BY je.branch.name, je.reference
""")
    List<Object[]> enterpriseAccountsReceivableMultiBranch(
            @Param("tenantId") UUID tenantId,
            @Param("start") LocalDateTime start,
            @Param("end") LocalDateTime end,
            @Param("arCodes") Set<String> arCodes
    );

    @Query("""
SELECT je.reference,
       SUM(
           CASE
               WHEN l.postedAt < :start AND l.direction = :credit THEN l.amount
               WHEN l.postedAt < :start AND l.direction = :debit THEN -l.amount
               ELSE 0
           END) as opening,
       SUM(
           CASE
               WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :credit THEN l.amount
               WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :debit THEN -l.amount
               ELSE 0
           END) as movement
FROM LedgerEntry l
JOIN l.journalEntry je
JOIN l.account a
WHERE je.tenantId = :tenantId
  AND a.code = :apCode
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
        END)
    +
    SUM(
        CASE
            WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :credit THEN l.amount
            WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :debit THEN -l.amount
            ELSE 0
        END)
) > 0
ORDER BY je.reference
""")
    List<Object[]> enterpriseAccountsPayable(
            @Param("tenantId") UUID tenantId,
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
           END) as opening,
       SUM(
           CASE
               WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :credit THEN l.amount
               WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :debit THEN -l.amount
               ELSE 0
           END) as movement
FROM LedgerEntry l
JOIN l.journalEntry je
JOIN l.account a
WHERE je.tenantId = :tenantId
  AND a.code IN :apCodes
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
        END)
    +
    SUM(
        CASE
            WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :credit THEN l.amount
            WHEN l.postedAt BETWEEN :start AND :end AND l.direction = :debit THEN -l.amount
            ELSE 0
        END)
) > 0
ORDER BY je.branch.name, je.reference
""")
    List<Object[]> enterpriseAccountsPayableMultiBranch(
            @Param("tenantId") UUID tenantId,
            @Param("start") LocalDateTime start,
            @Param("end") LocalDateTime end,
            @Param("apCodes") Set<String> apCodes
    );

    /*
     ------------------------------------------------------------
     STREAM LEDGER
     ------------------------------------------------------------
     */

    @Query("""
        SELECT l
        FROM LedgerEntry l
        JOIN l.journalEntry j
        WHERE j.tenantId = :tenantId
          AND j.branchId = :branchId
          AND j.posted = true
          AND j.reversed = false
        ORDER BY l.postedAt ASC, l.id ASC
    """)
    List<LedgerEntry> streamBranchLedger(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );

    /*
    ------------------------------------------------------------
    LEDGER DELTA (SINGLE BRANCH)
    ------------------------------------------------------------
    */

    @Query("""
        SELECT a.id,
               COALESCE(SUM(
                   CASE
                       WHEN a.type IN :debitNormal AND le.direction = :debit THEN le.amount
                       WHEN a.type IN :debitNormal AND le.direction = :credit THEN -le.amount
                       WHEN a.type IN :creditNormal AND le.direction = :credit THEN le.amount
                       WHEN a.type IN :creditNormal AND le.direction = :debit THEN -le.amount
                       ELSE 0
                   END
               ),0)
        FROM LedgerEntry le
        JOIN le.account a
        JOIN le.journalEntry je
        WHERE je.tenantId = :tenantId
          AND je.posted = true
          AND je.reversed = false
          AND le.postedAt BETWEEN :start AND :end
          AND (:branchId IS NULL OR je.branch.id = :branchId)
        GROUP BY a.id
    """)
    List<Object[]> ledgerDeltaByAccount(
            UUID tenantId,
            LocalDateTime start,
            LocalDateTime end,
            UUID branchId,
            Set<AccountType> debitNormal,
            Set<AccountType> creditNormal,
            EntryDirection debit,
            EntryDirection credit
    );

    @Query("""
    SELECT je.branch.name,
           a.id,
           COALESCE(SUM(
               CASE
                   WHEN a.type IN :debitNormal AND le.direction = :debit THEN le.amount
                   WHEN a.type IN :debitNormal AND le.direction = :credit THEN -le.amount
                   WHEN a.type IN :creditNormal AND le.direction = :credit THEN le.amount
                   WHEN a.type IN :creditNormal AND le.direction = :debit THEN -le.amount
                   ELSE 0
               END
           ),0)
    FROM LedgerEntry le
    JOIN le.account a
    JOIN le.journalEntry je
    WHERE je.tenantId = :tenantId
      AND je.posted = true
      AND je.reversed = false
      AND le.postedAt BETWEEN :start AND :end
    GROUP BY je.branch.name, a.id
""")
    List<Object[]> ledgerDeltaByAccountMultiBranch(
            UUID tenantId,
            LocalDateTime start,
            LocalDateTime end,
            Set<AccountType> debitNormal,
            Set<AccountType> creditNormal,
            EntryDirection debit,
            EntryDirection credit
    );
}