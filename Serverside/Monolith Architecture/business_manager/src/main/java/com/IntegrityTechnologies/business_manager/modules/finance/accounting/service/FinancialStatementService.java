package com.IntegrityTechnologies.business_manager.modules.finance.accounting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.snapshots.DailyAccountBalanceSnapshot;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.snapshots.DailyAccountBalanceSnapshotRepository;
import org.springframework.cache.annotation.Cacheable;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static com.IntegrityTechnologies.business_manager.modules.finance.accounting.support.AccountingSignRules.CREDIT_NORMAL;
import static com.IntegrityTechnologies.business_manager.modules.finance.accounting.support.AccountingSignRules.DEBIT_NORMAL;

@Transactional(readOnly = true)
@Service
@RequiredArgsConstructor
public class FinancialStatementService {

    private final LedgerEntryRepository ledgerRepo;
    private final DailyAccountBalanceSnapshotRepository snapshotRepo;
    private final AccountRepository accountRepo;

    /* ============================================================
       TRIAL BALANCE (Enterprise)
    ============================================================ */
    @Cacheable(
            value = "trialBalance",
            key = "{#branchId,#from,#to}"
    )
    public List<TrialBalanceRow> getTrialBalance(
            LocalDate from,
            LocalDate to,
            UUID branchId
    ) {

        LocalDateTime start = from.atStartOfDay();
        LocalDateTime end = to.atTime(23,59,59);

        Map<UUID, BigDecimal> openingMap =
                snapshotRepo.findByBranchIdAndSnapshotDate(branchId, from.minusDays(1))
                        .stream()
                        .collect(Collectors.toMap(
                                DailyAccountBalanceSnapshot::getAccountId,
                                DailyAccountBalanceSnapshot::getClosingBalance
                        ));

        Map<UUID, BigDecimal> deltaMap =
                ledgerRepo.ledgerDeltaByAccount(
                                start,end,branchId,
                                DEBIT_NORMAL,CREDIT_NORMAL,
                                EntryDirection.DEBIT,EntryDirection.CREDIT
                        ).stream()
                        .collect(Collectors.toMap(
                                r -> (UUID) r[0],
                                r -> safe((BigDecimal) r[1])
                        ));

        List<TrialBalanceRow> result = new ArrayList<>();

        Set<UUID> accounts = new HashSet<>();

        accounts.addAll(openingMap.keySet());
        accounts.addAll(deltaMap.keySet());

        for (UUID accountId : accounts) {

            BigDecimal opening = openingMap.getOrDefault(accountId, BigDecimal.ZERO);
            BigDecimal movement = deltaMap.getOrDefault(accountId, BigDecimal.ZERO);
            BigDecimal closing = opening.add(movement);

            result.add(
                    TrialBalanceRow.builder()
                            .code(accountId.toString())
                            .name(accountId.toString())
                            .opening(opening)
                            .debit(movement.max(BigDecimal.ZERO))
                            .credit(movement.min(BigDecimal.ZERO).abs())
                            .closing(closing)
                            .build()
            );
        }

        return result;
    }

    @Cacheable(
            value = "trialBalance",
            key = "{'MULTI',#from,#to}"
    )
    public List<TrialBalanceMultiBranchRow> getTrialBalanceMultiBranch(
            LocalDate from,
            LocalDate to
    ) {

        LocalDateTime start = from.atStartOfDay();
        LocalDateTime end = to.atTime(23,59,59);

        Map<String, List<TrialBalanceMultiBranchRow>> branchMap = new LinkedHashMap<>();

        List<Object[]> deltas =
                ledgerRepo.ledgerDeltaByAccountMultiBranch(
                        start,
                        end,
                        DEBIT_NORMAL,
                        CREDIT_NORMAL,
                        EntryDirection.DEBIT,
                        EntryDirection.CREDIT
                );

        for (Object[] r : deltas) {

            String branch = (String) r[0];
            UUID accountId = (UUID) r[1];
            BigDecimal movement = safe((BigDecimal) r[2]);

            branchMap.computeIfAbsent(branch, b -> new ArrayList<>())
                    .add(
                            TrialBalanceMultiBranchRow.builder()
                                    .branchName(branch)
                                    .code(accountId.toString())
                                    .name(accountId.toString())
                                    .opening(BigDecimal.ZERO)
                                    .debit(movement.max(BigDecimal.ZERO))
                                    .credit(movement.min(BigDecimal.ZERO).abs())
                                    .closing(movement)
                                    .build()
                    );
        }

        List<TrialBalanceMultiBranchRow> result = new ArrayList<>();

        for (var entry : branchMap.entrySet()) {

            BigDecimal branchDebit = BigDecimal.ZERO;
            BigDecimal branchCredit = BigDecimal.ZERO;

            result.add(
                    TrialBalanceMultiBranchRow.builder()
                            .branchName(entry.getKey())
                            .branchTotal(false)
                            .build()
            );

            for (var row : entry.getValue()) {

                result.add(row);

                branchDebit = branchDebit.add(row.getDebit());
                branchCredit = branchCredit.add(row.getCredit());
            }

            result.add(
                    TrialBalanceMultiBranchRow.builder()
                            .branchName(entry.getKey())
                            .debit(branchDebit)
                            .credit(branchCredit)
                            .branchTotal(true)
                            .build()
            );
        }

        return result;
    }

    public List<GeneralLedgerRow> getGeneralLedger(
            UUID accountId,
            LocalDate from,
            LocalDate to,
            UUID branchId
    ) {

        LocalDateTime start = from.atStartOfDay();
        LocalDateTime end = to.atTime(23, 59, 59);

        List<LedgerEntry> entries =
                ledgerRepo.findLedgerEntriesForGeneralLedger(
                        accountId,
                        end,
                        branchId
                );

        BigDecimal running = BigDecimal.ZERO;
        BigDecimal openingBalance = BigDecimal.ZERO;

        List<GeneralLedgerRow> result = new ArrayList<>();

        for (LedgerEntry le : entries) {

            BigDecimal signedAmount;

            if (DEBIT_NORMAL.contains(le.getAccount().getType())) {
                signedAmount = le.getDirection() == EntryDirection.DEBIT
                        ? le.getAmount()
                        : le.getAmount().negate();
            } else {
                signedAmount = le.getDirection() == EntryDirection.CREDIT
                        ? le.getAmount()
                        : le.getAmount().negate();
            }

            running = running.add(signedAmount);

            if (le.getPostedAt().isBefore(start)) {
                openingBalance = running;
                continue;
            }

            result.add(
                    GeneralLedgerRow.builder()
                            .postingDate(le.getPostedAt())
                            .description(le.getJournalEntry().getDescription())
                            .debit(le.getDirection() == EntryDirection.DEBIT ? le.getAmount() : BigDecimal.ZERO)
                            .credit(le.getDirection() == EntryDirection.CREDIT ? le.getAmount() : BigDecimal.ZERO)
                            .runningBalance(running)
                            .build()
            );
        }

        // Insert explicit opening row
        result.add(0,
                GeneralLedgerRow.builder()
                        .postingDate(start)
                        .description("Opening Balance")
                        .debit(BigDecimal.ZERO)
                        .credit(BigDecimal.ZERO)
                        .runningBalance(openingBalance)
                        .build()
        );

        return result;
    }

    public List<GeneralLedgerMultiBranchRow> getGeneralLedgerMultiBranch(
            UUID accountId,
            LocalDate from,
            LocalDate to
    ) {

        LocalDateTime start = from.atStartOfDay();
        LocalDateTime end = to.atTime(23,59,59);

        List<Object[]> rows =
                ledgerRepo.enterpriseGeneralLedgerMultiBranch(
                        accountId,
                        end
                );

        List<GeneralLedgerMultiBranchRow> result = new ArrayList<>();

        Map<String, BigDecimal> runningMap = new LinkedHashMap<>();

        for (Object[] r : rows) {

            String branch = (String) r[0];
            LocalDateTime postedAt = (LocalDateTime) r[1];
            String description = (String) r[2];
            EntryDirection direction = (EntryDirection) r[3];
            BigDecimal amount = safe((BigDecimal) r[4]);
            AccountType type = (AccountType) r[5];

            BigDecimal signed;

            if (DEBIT_NORMAL.contains(type)) {
                signed = direction == EntryDirection.DEBIT ? amount : amount.negate();
            } else {
                signed = direction == EntryDirection.CREDIT ? amount : amount.negate();
            }

            runningMap.putIfAbsent(branch, BigDecimal.ZERO);

            BigDecimal running = runningMap.get(branch).add(signed);
            runningMap.put(branch, running);

            if (postedAt.isBefore(start)) continue;

            result.add(
                    GeneralLedgerMultiBranchRow.builder()
                            .branchName(branch)
                            .postingDate(postedAt)
                            .description(description)
                            .debit(direction == EntryDirection.DEBIT ? amount : BigDecimal.ZERO)
                            .credit(direction == EntryDirection.CREDIT ? amount : BigDecimal.ZERO)
                            .runningBalance(running)
                            .build()
            );
        }

        return result;
    }

    @Cacheable(
            value = "profitLoss",
            key = "{#branchId,#from,#to}"
    )
    public List<ProfitAndLossRow> getProfitAndLoss(
            LocalDate from,
            LocalDate to,
            UUID branchId
    ) {

        LocalDateTime start = from.atStartOfDay();
        LocalDateTime end = to.atTime(23,59,59);

        List<Object[]> rows =
                ledgerRepo.netMovementByAccountType(
                        start,
                        end,
                        branchId,
                        DEBIT_NORMAL,
                        CREDIT_NORMAL,
                        Set.of(AccountType.INCOME, AccountType.EXPENSE),
                        EntryDirection.DEBIT,
                        EntryDirection.CREDIT
                );

        BigDecimal revenue = BigDecimal.ZERO;
        BigDecimal expense = BigDecimal.ZERO;

        for (Object[] r : rows) {

            AccountType type = (AccountType) r[0];
            BigDecimal amount = safe((BigDecimal) r[1]);

            if (type == AccountType.INCOME) {
                revenue = revenue.add(amount);
            } else {
                expense = expense.add(amount);
            }
        }

        List<ProfitAndLossRow> result = new ArrayList<>();

        result.add(ProfitAndLossRow.detail("Revenue","Revenue", revenue));
        result.add(ProfitAndLossRow.sectionTotal("Total Revenue", revenue));

        result.add(ProfitAndLossRow.detail("Expense","Expenses", expense));
        result.add(ProfitAndLossRow.sectionTotal("Total Expenses", expense));

        result.add(
                ProfitAndLossRow.grandTotal(
                        "Net Profit",
                        revenue.subtract(expense)
                )
        );

        return result;
    }

    @Cacheable(
            value = "profitLoss",
            key = "{'MULTI',#from,#to}"
    )
    public List<ProfitAndLossMultiBranchRow> getProfitAndLossMultiBranch(
            LocalDate from,
            LocalDate to
    ) {

        LocalDateTime start = from.atStartOfDay();
        LocalDateTime end = to.atTime(23,59,59);

        List<Object[]> rows =
                ledgerRepo.enterpriseProfitAndLossMultiBranch(
                        start,
                        end,
                        Set.of(AccountType.INCOME, AccountType.EXPENSE),
                        AccountType.INCOME,
                        AccountType.EXPENSE
                );

        Map<String, List<Object[]>> branchMap =
                rows.stream()
                        .collect(Collectors.groupingBy(
                                r -> (String) r[0],
                                LinkedHashMap::new,
                                Collectors.toList()
                        ));

        List<ProfitAndLossMultiBranchRow> result = new ArrayList<>();

        for (var entry : branchMap.entrySet()) {

            String branch = entry.getKey();

            result.add(
                    ProfitAndLossMultiBranchRow.builder()
                            .branchName(branch)
                            .branchHeader(true)
                            .build()
            );

            BigDecimal branchTotal = BigDecimal.ZERO;

            for (Object[] r : entry.getValue()) {

                AccountType type = (AccountType) r[1];
                BigDecimal amount = safe((BigDecimal) r[3]);

                result.add(
                        ProfitAndLossMultiBranchRow.builder()
                                .branchName(branch)
                                .label(type.name())
                                .amount(amount)
                                .build()
                );

                branchTotal = branchTotal.add(amount);
            }

            result.add(
                    ProfitAndLossMultiBranchRow.builder()
                            .branchName(branch)
                            .amount(branchTotal)
                            .branchTotal(true)
                            .build()
            );
        }

        return result;
    }

    @Cacheable(
            value = "balanceSheet",
            key = "{#branchId,#asAt}"
    )
    public List<BalanceSheetRow> getBalanceSheet(
            LocalDate asAt,
            UUID branchId
    ) {

        Map<UUID, BigDecimal> snapshotMap =
                snapshotRepo.findByBranchIdAndSnapshotDate(branchId, asAt)
                        .stream()
                        .collect(Collectors.toMap(
                                DailyAccountBalanceSnapshot::getAccountId,
                                DailyAccountBalanceSnapshot::getClosingBalance
                        ));

        List<BalanceSheetRow> result = new ArrayList<>();

        BigDecimal total = BigDecimal.ZERO;

        for (var entry : snapshotMap.entrySet()) {

            BigDecimal balance = entry.getValue();

            result.add(
                    BalanceSheetRow.builder()
                            .code(entry.getKey().toString())
                            .name(entry.getKey().toString())
                            .balance(balance)
                            .build()
            );

            total = total.add(balance);
        }

        result.add(
                BalanceSheetRow.builder()
                        .balance(total)
                        .grandTotal(true)
                        .build()
        );

        return result;
    }

    @Cacheable(
            value = "balanceSheet",
            key = "{'MULTI',#asAt}"
    )
    public List<BalanceSheetMultiBranchRow> getBalanceSheetMultiBranch(
            LocalDate asAt
    ) {

        List<DailyAccountBalanceSnapshot> snaps =
                snapshotRepo.findBySnapshotDate(asAt);

        Map<String, List<DailyAccountBalanceSnapshot>> grouped =
                snaps.stream()
                        .collect(Collectors.groupingBy(
                                s -> s.getBranchId().toString()
                        ));

        List<BalanceSheetMultiBranchRow> result = new ArrayList<>();

        for (var entry : grouped.entrySet()) {

            String branch = entry.getKey();

            result.add(
                    BalanceSheetMultiBranchRow.builder()
                            .branchName(branch)
                            .branchHeader(true)
                            .build()
            );

            BigDecimal branchTotal = BigDecimal.ZERO;

            for (var s : entry.getValue()) {

                result.add(
                        BalanceSheetMultiBranchRow.builder()
                                .branchName(branch)
                                .code(s.getAccountId().toString())
                                .name(s.getAccountId().toString())
                                .balance(s.getClosingBalance())
                                .build()
                );

                branchTotal = branchTotal.add(s.getClosingBalance());
            }

            result.add(
                    BalanceSheetMultiBranchRow.builder()
                            .branchName(branch)
                            .balance(branchTotal)
                            .branchTotal(true)
                            .build()
            );
        }

        return result;
    }

    @Cacheable(
            value = "cashFlow",
            key = "{#branchId,#from,#to}"
    )
    public List<CashFlowRow> getCashFlow(
            LocalDate from,
            LocalDate to,
            UUID branchId
    ) {

        LocalDateTime start = from.atStartOfDay();
        LocalDateTime end = to.atTime(23,59,59);

        List<Object[]> rows =
                ledgerRepo.enterpriseCashFlow(
                        start,
                        end,
                        branchId,
                        Set.of("1000","1100","1150"),
                        EntryDirection.DEBIT,
                        EntryDirection.CREDIT
                );

        List<CashFlowRow> result = new ArrayList<>();
        BigDecimal total = BigDecimal.ZERO;

        for (Object[] r : rows) {

            String code = (String) r[1];
            String name = (String) r[2];
            BigDecimal movement = safe((BigDecimal) r[3]);

            result.add(
                    CashFlowRow.builder()
                            .code(code)
                            .name(name)
                            .netMovement(movement)
                            .build()
            );

            total = total.add(movement);
        }

        result.add(
                CashFlowRow.builder()
                        .name("TOTAL CASH MOVEMENT")
                        .netMovement(total)
                        .total(true)
                        .build()
        );

        return result;
    }

    @Cacheable(
            value = "cashFlow",
            key = "{'MULTI',#from,#to}"
    )
    public List<CashFlowMultiBranchRow> getCashFlowMultiBranch(
            LocalDate from,
            LocalDate to
    ) {

        LocalDateTime start = from.atStartOfDay();
        LocalDateTime end = to.atTime(23,59,59);

        List<Object[]> rows =
                ledgerRepo.enterpriseCashFlowMultiBranch(
                        start,
                        end,
                        Set.of("1000","1100","1150"),
                        EntryDirection.DEBIT,
                        EntryDirection.CREDIT
                );

        Map<String, List<Object[]>> branchMap =
                rows.stream()
                        .collect(Collectors.groupingBy(
                                r -> (String) r[0],
                                LinkedHashMap::new,
                                Collectors.toList()
                        ));

        List<CashFlowMultiBranchRow> result = new ArrayList<>();

        for (var entry : branchMap.entrySet()) {

            String branch = entry.getKey();

            result.add(
                    CashFlowMultiBranchRow.builder()
                            .branchName(branch)
                            .branchHeader(true)
                            .build()
            );

            BigDecimal branchTotal = BigDecimal.ZERO;

            for (Object[] r : entry.getValue()) {

                BigDecimal movement = safe((BigDecimal) r[3]);

                result.add(
                        CashFlowMultiBranchRow.builder()
                                .branchName(branch)
                                .code((String) r[1])
                                .name((String) r[2])
                                .netMovement(movement)
                                .build()
                );

                branchTotal = branchTotal.add(movement);
            }

            result.add(
                    CashFlowMultiBranchRow.builder()
                            .branchName(branch)
                            .netMovement(branchTotal)
                            .branchTotal(true)
                            .build()
            );
        }

        return result;
    }

    @Cacheable(value = "accountsReceivable",
            key = "{#branchId,#from,#to}")
    public List<AccountsReceivableRow> getAccountsReceivable(
            LocalDate from,
            LocalDate to,
            UUID branchId
    ) {

        LocalDateTime start = from.atStartOfDay();
        LocalDateTime end = to.atTime(23,59,59);

        List<Object[]> rows =
                ledgerRepo.enterpriseAccountsReceivable(
                        start,
                        end,
                        branchId,
                        "1500"
                );

        List<AccountsReceivableRow> result = new ArrayList<>();
        BigDecimal total = BigDecimal.ZERO;

        for (Object[] r : rows) {

            String reference = (String) r[0];
            BigDecimal opening = safe((BigDecimal) r[1]);
            BigDecimal movement = safe((BigDecimal) r[2]);

            BigDecimal closing = opening.add(movement);
            total = total.add(closing);

            result.add(
                    AccountsReceivableRow.builder()
                            .reference(reference)
                            .balance(closing)
                            .build()
            );
        }

        result.add(
                AccountsReceivableRow.builder()
                        .reference("TOTAL RECEIVABLE")
                        .balance(total)
                        .total(true)
                        .build()
        );

        return result;
    }

    @Cacheable(
            value = "accountsReceivable",
            key = "{'MULTI',#from,#to}"
    )
    public List<AccountsReceivableMultiBranchRow> getAccountsReceivableMultiBranch(
            LocalDate from,
            LocalDate to
    ) {

        LocalDateTime start = from.atStartOfDay();
        LocalDateTime end = to.atTime(23,59,59);

        List<Object[]> rows =
                ledgerRepo.enterpriseAccountsReceivableMultiBranch(
                        start,
                        end,
                        "1500"
                );

        Map<String, List<Object[]>> branchMap =
                rows.stream()
                        .collect(Collectors.groupingBy(
                                r -> (String) r[0],
                                LinkedHashMap::new,
                                Collectors.toList()
                        ));

        List<AccountsReceivableMultiBranchRow> result = new ArrayList<>();
        BigDecimal consolidated = BigDecimal.ZERO;

        for (var entry : branchMap.entrySet()) {

            String branchName = entry.getKey();

            result.add(
                    AccountsReceivableMultiBranchRow.builder()
                            .branchName(branchName)
                            .branchHeader(true)
                            .build()
            );

            BigDecimal branchTotal = BigDecimal.ZERO;

            for (Object[] r : entry.getValue()) {

                String reference = (String) r[1];
                BigDecimal opening = safe((BigDecimal) r[2]);
                BigDecimal movement = safe((BigDecimal) r[3]);

                BigDecimal closing = opening.add(movement);
                branchTotal = branchTotal.add(closing);

                result.add(
                        AccountsReceivableMultiBranchRow.builder()
                                .branchName(branchName)
                                .reference(reference)
                                .balance(closing)
                                .build()
                );
            }

            consolidated = consolidated.add(branchTotal);

            result.add(
                    AccountsReceivableMultiBranchRow.builder()
                            .branchName(branchName)
                            .balance(branchTotal)
                            .branchTotal(true)
                            .build()
            );
        }

        result.add(
                AccountsReceivableMultiBranchRow.builder()
                        .balance(consolidated)
                        .consolidatedTotal(true)
                        .build()
        );

        return result;
    }

    @Cacheable(value = "accountsPayable",
            key = "{#branchId,#from,#to}")
    public List<AccountsPayableRow> getAccountsPayable(
            LocalDate from,
            LocalDate to,
            UUID branchId
    ) {

        LocalDateTime start = from.atStartOfDay();
        LocalDateTime end = to.atTime(23,59,59);

        List<Object[]> rows =
                ledgerRepo.enterpriseAccountsPayable(
                        start,
                        end,
                        branchId,
                        "2000"
                );

        List<AccountsPayableRow> result = new ArrayList<>();
        BigDecimal total = BigDecimal.ZERO;

        for (Object[] r : rows) {

            String reference = (String) r[0];
            BigDecimal opening = safe((BigDecimal) r[1]);
            BigDecimal movement = safe((BigDecimal) r[2]);

            BigDecimal closing = opening.add(movement);
            total = total.add(closing);

            result.add(
                    AccountsPayableRow.builder()
                            .reference(reference)
                            .balance(closing)
                            .build()
            );
        }

        result.add(
                AccountsPayableRow.builder()
                        .reference("TOTAL PAYABLE")
                        .balance(total)
                        .total(true)
                        .build()
        );

        return result;
    }

    @Cacheable(
            value = "accountsPayable",
            key = "{'MULTI',#from,#to}"
    )
    public List<AccountsPayableMultiBranchRow> getAccountsPayableMultiBranch(
            LocalDate from,
            LocalDate to
    ) {

        LocalDateTime start = from.atStartOfDay();
        LocalDateTime end = to.atTime(23,59,59);

        List<Object[]> rows =
                ledgerRepo.enterpriseAccountsPayableMultiBranch(
                        start,
                        end,
                        "2000"
                );

        Map<String, List<Object[]>> branchMap =
                rows.stream()
                        .collect(Collectors.groupingBy(
                                r -> (String) r[0],
                                LinkedHashMap::new,
                                Collectors.toList()
                        ));

        List<AccountsPayableMultiBranchRow> result = new ArrayList<>();
        BigDecimal consolidated = BigDecimal.ZERO;

        for (var entry : branchMap.entrySet()) {

            String branchName = entry.getKey();

            result.add(
                    AccountsPayableMultiBranchRow.builder()
                            .branchName(branchName)
                            .branchHeader(true)
                            .build()
            );

            BigDecimal branchTotal = BigDecimal.ZERO;

            for (Object[] r : entry.getValue()) {

                String reference = (String) r[1];
                BigDecimal opening = safe((BigDecimal) r[2]);
                BigDecimal movement = safe((BigDecimal) r[3]);

                BigDecimal closing = opening.add(movement);
                branchTotal = branchTotal.add(closing);

                result.add(
                        AccountsPayableMultiBranchRow.builder()
                                .branchName(branchName)
                                .reference(reference)
                                .balance(closing)
                                .build()
                );
            }

            consolidated = consolidated.add(branchTotal);

            result.add(
                    AccountsPayableMultiBranchRow.builder()
                            .branchName(branchName)
                            .balance(branchTotal)
                            .branchTotal(true)
                            .build()
            );
        }

        result.add(
                AccountsPayableMultiBranchRow.builder()
                        .balance(consolidated)
                        .consolidatedTotal(true)
                        .build()
        );

        return result;
    }



    /* ============================================================
       DTOs AND HELPER
    ============================================================ */


    private BigDecimal safe(BigDecimal val) {
        return val == null ? BigDecimal.ZERO : val;
    }

    /* ============================================================
       DTO
    ============================================================ */
    @Data
    @Builder
    public static class TrialBalanceRow {
        private String code;
        private String name;
        private BigDecimal opening;
        private BigDecimal debit;
        private BigDecimal credit;
        private BigDecimal closing;
    }

    @Data
    @Builder
    public static class TrialBalanceMultiBranchRow {

        private String branchName;
        private String code;
        private String name;

        private BigDecimal opening;
        private BigDecimal debit;
        private BigDecimal credit;
        private BigDecimal closing;

        private boolean branchTotal;
        private boolean consolidatedTotal;
    }

    @Data
    @Builder
    public static class GeneralLedgerRow {

        private LocalDateTime postingDate;
        private String description;
        private BigDecimal debit;
        private BigDecimal credit;
        private BigDecimal runningBalance;
    }

    @Data
    @Builder
    public static class GeneralLedgerMultiBranchRow {

        private String branchName;
        private LocalDateTime postingDate;
        private String description;
        private BigDecimal debit;
        private BigDecimal credit;
        private BigDecimal runningBalance;

        private boolean branchHeader;
        private boolean branchTotal;
    }
    @Data
    @Builder
    public static class ProfitAndLossRow {

        private String section;     // Revenue / Expense
        private String label;       // Account name or total label
        private BigDecimal amount;
        private boolean bold;
        private boolean grandTotal;

        public static ProfitAndLossRow detail(String section, String label, BigDecimal amount) {
            return ProfitAndLossRow.builder()
                    .section(section)
                    .label(label)
                    .amount(amount)
                    .bold(false)
                    .grandTotal(false)
                    .build();
        }

        public static ProfitAndLossRow sectionTotal(String label, BigDecimal amount) {
            return ProfitAndLossRow.builder()
                    .section("")
                    .label(label)
                    .amount(amount)
                    .bold(true)
                    .grandTotal(false)
                    .build();
        }

        public static ProfitAndLossRow grandTotal(String label, BigDecimal amount) {
            return ProfitAndLossRow.builder()
                    .section("")
                    .label(label)
                    .amount(amount)
                    .bold(true)
                    .grandTotal(true)
                    .build();
        }
    }

    @Data
    @Builder
    public static class ProfitAndLossMultiBranchRow {

        private String branchName;
        private String label;
        private BigDecimal amount;

        private boolean branchHeader;
        private boolean branchTotal;
        private boolean consolidatedTotal;
    }

    @Data
    @Builder
    public static class BalanceSheetRow {

        private String section;       // ASSET / LIABILITY / EQUITY
        private String code;
        private String name;
        private BigDecimal balance;

        private boolean sectionHeader;
        private boolean sectionTotal;
        private boolean grandTotal;
    }

    @Data
    @Builder
    public static class BalanceSheetMultiBranchRow {

        private String branchName;

        private String section;
        private String code;
        private String name;
        private BigDecimal balance;

        private boolean branchHeader;
        private boolean branchTotal;
        private boolean consolidatedTotal;
    }

    @Data
    @Builder
    public static class CashFlowRow {
        private String code;
        private String name;
        private BigDecimal netMovement;
        private boolean total;
    }

    @Data
    @Builder
    public static class CashFlowMultiBranchRow {

        private String branchName;
        private String code;
        private String name;
        private BigDecimal netMovement;

        private boolean branchHeader;
        private boolean branchTotal;
        private boolean consolidatedTotal;
    }

    @Data
    @Builder
    public static class AccountsReceivableRow {

        private String reference;
        private BigDecimal balance;
        private boolean total;
    }

    @Data
    @Builder
    public static class AccountsReceivableMultiBranchRow {

        private String branchName;
        private String reference;
        private BigDecimal balance;

        private boolean branchHeader;
        private boolean branchTotal;
        private boolean consolidatedTotal;
    }

    @Data
    @Builder
    public static class AccountsPayableRow {

        private String reference;
        private BigDecimal balance;
        private boolean total;
    }

    @Data
    @Builder
    public static class AccountsPayableMultiBranchRow {

        private String branchName;
        private String reference;
        private BigDecimal balance;

        private boolean branchHeader;
        private boolean branchTotal;
        private boolean consolidatedTotal;
    }
}