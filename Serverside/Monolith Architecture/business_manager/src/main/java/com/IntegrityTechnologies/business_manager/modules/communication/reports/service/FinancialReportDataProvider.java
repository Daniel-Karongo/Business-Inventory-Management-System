package com.IntegrityTechnologies.business_manager.modules.communication.reports.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.FinancialStatementService;
import lombok.RequiredArgsConstructor;
import net.sf.jasperreports.engine.data.JRBeanCollectionDataSource;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class FinancialReportDataProvider {

    private final FinancialStatementService financialStatementService;

    public boolean supports(String reportName) {
        return reportName.startsWith("trial_balance")
                || reportName.startsWith("general_ledger")
                || reportName.startsWith("profit_and_loss")
                || reportName.startsWith("balance_sheet")
                || reportName.startsWith("cash_flow_statement")
                || reportName.startsWith("accounts_receivable_summary")
                || reportName.startsWith("accounts_payable_summary");
    }

    public JRBeanCollectionDataSource provide(
            String reportName,
            Map<String, Object> params
    ) {

        boolean branchProvided =
                params.containsKey("BRANCH_ID")
                        && params.get("BRANCH_ID") != null;

        LocalDate from = params.get("FROM_DATE") != null
                ? LocalDate.parse(params.get("FROM_DATE").toString())
                : null;

        LocalDate to = params.get("TO_DATE") != null
                ? LocalDate.parse(params.get("TO_DATE").toString())
                : null;

        LocalDate asAt = params.get("AS_AT_DATE") != null
                ? LocalDate.parse(params.get("AS_AT_DATE").toString())
                : null;

        UUID branchId = branchProvided
                ? UUID.fromString(params.get("BRANCH_ID").toString())
                : null;

        UUID accountId = params.get("ACCOUNT_ID") != null
                ? UUID.fromString(params.get("ACCOUNT_ID").toString())
                : null;

        List<?> data;

        switch (reportName) {

            case "trial_balance" -> {
                data = branchProvided
                        ? financialStatementService.getTrialBalance(from, to, branchId)
                        : financialStatementService.getTrialBalanceMultiBranch(from, to);
            }

            case "general_ledger" -> {
                data = branchProvided
                        ? financialStatementService.getGeneralLedger(accountId, from, to, branchId)
                        : financialStatementService.getGeneralLedgerMultiBranch(accountId, from, to);
            }

            case "profit_and_loss" -> {
                data = branchProvided
                        ? financialStatementService.getProfitAndLoss(from, to, branchId)
                        : financialStatementService.getProfitAndLossMultiBranch(from, to);
            }

            case "balance_sheet" -> {
                data = branchProvided
                        ? financialStatementService.getBalanceSheet(asAt, branchId)
                        : financialStatementService.getBalanceSheetMultiBranch(asAt);
            }

            case "cash_flow_statement" -> {
                data = branchProvided
                        ? financialStatementService.getCashFlow(from, to, branchId)
                        : financialStatementService.getCashFlowMultiBranch(from, to);
            }

            case "accounts_receivable_summary" -> {
                data = branchProvided
                        ? financialStatementService.getAccountsReceivable(from, to, branchId)
                        : financialStatementService.getAccountsReceivableMultiBranch(from, to);
            }

            case "accounts_payable_summary" -> {
                data = branchProvided
                        ? financialStatementService.getAccountsPayable(from, to, branchId)
                        : financialStatementService.getAccountsPayableMultiBranch(from, to);
            }

            default -> throw new IllegalArgumentException(
                    "Unsupported financial report: " + reportName
            );
        }

        return new JRBeanCollectionDataSource(data);
    }
}