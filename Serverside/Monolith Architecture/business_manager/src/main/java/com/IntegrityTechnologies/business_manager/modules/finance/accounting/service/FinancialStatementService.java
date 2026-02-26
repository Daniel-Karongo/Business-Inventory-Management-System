package com.IntegrityTechnologies.business_manager.modules.finance.accounting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static com.IntegrityTechnologies.business_manager.modules.finance.accounting.support.AccountingSignRules.CREDIT_NORMAL;
import static com.IntegrityTechnologies.business_manager.modules.finance.accounting.support.AccountingSignRules.DEBIT_NORMAL;

@Service
@RequiredArgsConstructor
public class FinancialStatementService {

    private final LedgerEntryRepository ledgerRepo;

    /* ============================================================
       TRIAL BALANCE (Enterprise)
    ============================================================ */
    public List<TrialBalanceRow> getTrialBalance(
            LocalDate from,
            LocalDate to,
            UUID branchId
    ) {

        LocalDateTime start = from.atStartOfDay();
        LocalDateTime end = to.atTime(23, 59, 59);

        List<Object[]> rows =
                ledgerRepo.enterpriseTrialBalance(
                        start,
                        end,
                        branchId,
                        DEBIT_NORMAL,
                        CREDIT_NORMAL,
                        EntryDirection.DEBIT,
                        EntryDirection.CREDIT
                );

        List<TrialBalanceRow> result = new ArrayList<>();

        for (Object[] r : rows) {

            String code = (String) r[0];
            String name = (String) r[1];
            BigDecimal opening = safe((BigDecimal) r[2]);
            BigDecimal debit = safe((BigDecimal) r[3]);
            BigDecimal credit = safe((BigDecimal) r[4]);

            BigDecimal closing =
                    opening.add(debit).subtract(credit);

            result.add(
                    TrialBalanceRow.builder()
                            .code(code)
                            .name(name)
                            .opening(opening)
                            .debit(debit)
                            .credit(credit)
                            .closing(closing)
                            .build()
            );
        }

        return result;
    }

    public List<TrialBalanceMultiBranchRow> getTrialBalanceMultiBranch(
            LocalDate from,
            LocalDate to
    ) {

        LocalDateTime start = from.atStartOfDay();
        LocalDateTime end = to.atTime(23, 59, 59);

        List<Object[]> rows =
                ledgerRepo.enterpriseTrialBalanceMultiBranch(
                        start,
                        end,
                        DEBIT_NORMAL,
                        CREDIT_NORMAL,
                        EntryDirection.DEBIT,
                        EntryDirection.CREDIT
                );

        Map<String, List<TrialBalanceMultiBranchRow>> branchMap = new LinkedHashMap<>();

        for (Object[] r : rows) {

            String branch = (String) r[0];
            String code = (String) r[1];
            String name = (String) r[2];
            BigDecimal opening = safe((BigDecimal) r[3]);
            BigDecimal debit = safe((BigDecimal) r[4]);
            BigDecimal credit = safe((BigDecimal) r[5]);
            BigDecimal closing = opening.add(debit).subtract(credit);

            branchMap.computeIfAbsent(branch, k -> new ArrayList<>())
                    .add(
                            TrialBalanceMultiBranchRow.builder()
                                    .branchName(branch)
                                    .code(code)
                                    .name(name)
                                    .opening(opening)
                                    .debit(debit)
                                    .credit(credit)
                                    .closing(closing)
                                    .build()
                    );
        }

        List<TrialBalanceMultiBranchRow> result = new ArrayList<>();

        BigDecimal consOpen = BigDecimal.ZERO;
        BigDecimal consDebit = BigDecimal.ZERO;
        BigDecimal consCredit = BigDecimal.ZERO;
        BigDecimal consClose = BigDecimal.ZERO;

        for (var entry : branchMap.entrySet()) {

            BigDecimal branchOpen = BigDecimal.ZERO;
            BigDecimal branchDebit = BigDecimal.ZERO;
            BigDecimal branchCredit = BigDecimal.ZERO;
            BigDecimal branchClose = BigDecimal.ZERO;

            result.add(
                    TrialBalanceMultiBranchRow.builder()
                            .branchName(entry.getKey())
                            .branchTotal(false)
                            .build()
            );

            for (var row : entry.getValue()) {

                result.add(row);

                branchOpen = branchOpen.add(row.getOpening());
                branchDebit = branchDebit.add(row.getDebit());
                branchCredit = branchCredit.add(row.getCredit());
                branchClose = branchClose.add(row.getClosing());
            }

            result.add(
                    TrialBalanceMultiBranchRow.builder()
                            .branchName(entry.getKey())
                            .opening(branchOpen)
                            .debit(branchDebit)
                            .credit(branchCredit)
                            .closing(branchClose)
                            .branchTotal(true)
                            .build()
            );

            consOpen = consOpen.add(branchOpen);
            consDebit = consDebit.add(branchDebit);
            consCredit = consCredit.add(branchCredit);
            consClose = consClose.add(branchClose);
        }

        result.add(
                TrialBalanceMultiBranchRow.builder()
                        .opening(consOpen)
                        .debit(consDebit)
                        .credit(consCredit)
                        .closing(consClose)
                        .consolidatedTotal(true)
                        .build()
        );

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

    public List<ProfitAndLossRow> getProfitAndLoss(
            LocalDate from,
            LocalDate to,
            UUID branchId
    ) {

        LocalDateTime start = from.atStartOfDay();
        LocalDateTime end = to.atTime(23, 59, 59);

        List<Object[]> rows =
                ledgerRepo.enterpriseProfitAndLoss(
                        start,
                        end,
                        branchId,
                        Set.of(AccountType.INCOME, AccountType.EXPENSE),
                        AccountType.INCOME,
                        AccountType.EXPENSE,
                        EntryDirection.DEBIT,
                        EntryDirection.CREDIT
                );

        Map<String, BigDecimal> revenueMap = new LinkedHashMap<>();
        Map<String, BigDecimal> expenseMap = new LinkedHashMap<>();

        for (Object[] r : rows) {

            String type = (String) r[0];   // INCOME or EXPENSE
            String name = (String) r[1];
            BigDecimal amount = safe((BigDecimal) r[2]);

            if ("INCOME".equals(type)) {
                revenueMap.put(name, amount);
            } else if ("EXPENSE".equals(type)) {
                expenseMap.put(name, amount);
            }
        }

        List<ProfitAndLossRow> result = new ArrayList<>();

        BigDecimal totalRevenue = BigDecimal.ZERO;
        for (var e : revenueMap.entrySet()) {
            totalRevenue = totalRevenue.add(e.getValue());
            result.add(ProfitAndLossRow.detail("Revenue", e.getKey(), e.getValue()));
        }

        result.add(ProfitAndLossRow.sectionTotal("Total Revenue", totalRevenue));

        BigDecimal totalExpense = BigDecimal.ZERO;
        for (var e : expenseMap.entrySet()) {
            totalExpense = totalExpense.add(e.getValue());
            result.add(ProfitAndLossRow.detail("Expense", e.getKey(), e.getValue()));
        }

        result.add(ProfitAndLossRow.sectionTotal("Total Expenses", totalExpense));

        BigDecimal netProfit = totalRevenue.subtract(totalExpense);

        result.add(ProfitAndLossRow.grandTotal("Net Profit", netProfit));

        return result;
    }

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
        BigDecimal consolidated = BigDecimal.ZERO;

        for (var entry : branchMap.entrySet()) {

            String branchName = entry.getKey();
            result.add(
                    ProfitAndLossMultiBranchRow.builder()
                            .branchName(branchName)
                            .branchHeader(true)
                            .build()
            );

            BigDecimal branchTotal = BigDecimal.ZERO;

            for (Object[] r : entry.getValue()) {

                AccountType type = (AccountType) r[1];
                String accountName = (String) r[2];
                BigDecimal amount = safe((BigDecimal) r[3]);

                branchTotal = branchTotal.add(amount);

                result.add(
                        ProfitAndLossMultiBranchRow.builder()
                                .branchName(branchName)
                                .label(accountName)
                                .amount(amount)
                                .build()
                );
            }

            consolidated = consolidated.add(branchTotal);

            result.add(
                    ProfitAndLossMultiBranchRow.builder()
                            .branchName(branchName)
                            .amount(branchTotal)
                            .branchTotal(true)
                            .build()
            );
        }

        result.add(
                ProfitAndLossMultiBranchRow.builder()
                        .label("CONSOLIDATED NET PROFIT")
                        .amount(consolidated)
                        .consolidatedTotal(true)
                        .build()
        );

        return result;
    }

    public List<BalanceSheetRow> getBalanceSheet(
            LocalDate asAt,
            UUID branchId
    ) {

        LocalDateTime end = asAt.atTime(23,59,59);

        List<Object[]> rows =
                ledgerRepo.enterpriseBalanceSheet(
                        end,
                        branchId,
                        Set.of(AccountType.ASSET, AccountType.LIABILITY, AccountType.EQUITY),
                        DEBIT_NORMAL,
                        CREDIT_NORMAL,
                        EntryDirection.DEBIT,
                        EntryDirection.CREDIT
                );

        Map<String, List<Object[]>> grouped =
                rows.stream()
                        .collect(Collectors.groupingBy(r -> (String) r[0]));

        List<BalanceSheetRow> result = new ArrayList<>();

        BigDecimal grandTotal = BigDecimal.ZERO;

        for (String section : List.of("ASSET","LIABILITY","EQUITY")) {

            List<Object[]> sectionRows = grouped.get(section);
            if (sectionRows == null) continue;

            result.add(
                    BalanceSheetRow.builder()
                            .section(section)
                            .sectionHeader(true)
                            .build()
            );

            BigDecimal sectionTotal = BigDecimal.ZERO;

            for (Object[] r : sectionRows) {

                String code = (String) r[1];
                String name = (String) r[2];
                BigDecimal balance = safe((BigDecimal) r[3]);

                result.add(
                        BalanceSheetRow.builder()
                                .section(section)
                                .code(code)
                                .name(name)
                                .balance(balance)
                                .build()
                );

                sectionTotal = sectionTotal.add(balance);
            }

            result.add(
                    BalanceSheetRow.builder()
                            .section(section)
                            .balance(sectionTotal)
                            .sectionTotal(true)
                            .build()
            );

            grandTotal = grandTotal.add(sectionTotal);
        }

        result.add(
                BalanceSheetRow.builder()
                        .balance(grandTotal)
                        .grandTotal(true)
                        .build()
        );

        return result;
    }

    public List<BalanceSheetMultiBranchRow> getBalanceSheetMultiBranch(
            LocalDate asAt
    ) {

        LocalDateTime end = asAt.atTime(23,59,59);

        List<Object[]> rows =
                ledgerRepo.enterpriseBalanceSheetMultiBranch(
                        end,
                        Set.of(AccountType.ASSET, AccountType.LIABILITY, AccountType.EQUITY),
                        DEBIT_NORMAL,
                        CREDIT_NORMAL,
                        EntryDirection.DEBIT,
                        EntryDirection.CREDIT
                );

        Map<String, List<Object[]>> branchMap =
                rows.stream()
                        .collect(Collectors.groupingBy(r -> (String) r[0],
                                LinkedHashMap::new,
                                Collectors.toList()));

        List<BalanceSheetMultiBranchRow> result = new ArrayList<>();

        BigDecimal consolidated = BigDecimal.ZERO;

        for (var entry : branchMap.entrySet()) {

            String branchName = entry.getKey();

            result.add(
                    BalanceSheetMultiBranchRow.builder()
                            .branchName(branchName)
                            .branchHeader(true)
                            .build()
            );

            BigDecimal branchTotal = BigDecimal.ZERO;

            for (Object[] r : entry.getValue()) {

                String section = (String) r[1];
                String code = (String) r[2];
                String name = (String) r[3];
                BigDecimal balance = safe((BigDecimal) r[4]);

                branchTotal = branchTotal.add(balance);

                result.add(
                        BalanceSheetMultiBranchRow.builder()
                                .branchName(branchName)
                                .section(section)
                                .code(code)
                                .name(name)
                                .balance(balance)
                                .build()
                );
            }

            result.add(
                    BalanceSheetMultiBranchRow.builder()
                            .branchName(branchName)
                            .balance(branchTotal)
                            .branchTotal(true)
                            .build()
            );

            consolidated = consolidated.add(branchTotal);
        }

        result.add(
                BalanceSheetMultiBranchRow.builder()
                        .balance(consolidated)
                        .consolidatedTotal(true)
                        .build()
        );

        return result;
    }

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
        BigDecimal consolidated = BigDecimal.ZERO;

        for (var entry : branchMap.entrySet()) {

            String branchName = entry.getKey();
            result.add(
                    CashFlowMultiBranchRow.builder()
                            .branchName(branchName)
                            .branchHeader(true)
                            .build()
            );

            BigDecimal branchTotal = BigDecimal.ZERO;

            for (Object[] r : entry.getValue()) {

                String code = (String) r[1];
                String name = (String) r[2];
                BigDecimal movement = safe((BigDecimal) r[3]);

                branchTotal = branchTotal.add(movement);

                result.add(
                        CashFlowMultiBranchRow.builder()
                                .branchName(branchName)
                                .code(code)
                                .name(name)
                                .netMovement(movement)
                                .build()
                );
            }

            consolidated = consolidated.add(branchTotal);

            result.add(
                    CashFlowMultiBranchRow.builder()
                            .branchName(branchName)
                            .netMovement(branchTotal)
                            .branchTotal(true)
                            .build()
            );
        }

        result.add(
                CashFlowMultiBranchRow.builder()
                        .netMovement(consolidated)
                        .consolidatedTotal(true)
                        .build()
        );

        return result;
    }

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