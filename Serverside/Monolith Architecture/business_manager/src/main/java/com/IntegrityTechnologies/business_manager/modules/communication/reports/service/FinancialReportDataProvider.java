package com.IntegrityTechnologies.business_manager.modules.communication.reports.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.FinancialStatementService;
import lombok.RequiredArgsConstructor;
import net.sf.jasperreports.engine.data.JRBeanCollectionDataSource;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.util.Map;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class FinancialReportDataProvider {

    private final FinancialStatementService financialStatementService;

    public boolean supports(String reportName) {
        return switch (reportName) {
            case "trial_balance",
                    "general_ledger",
                    "profit_and_loss",
                    "balance_sheet",
                    "cash_flow_statement",
                    "accounts_receivable_summary",
                    "accounts_payable_summary" -> true;
            default -> false;
        };
    }

    public JRBeanCollectionDataSource provide(
            String reportName,
            Map<String, Object> params
    ) {

        String mode = params.getOrDefault("REPORT_MODE", "SINGLE").toString();

        UUID branchId = null;
        if (params.containsKey("BRANCH_ID") && params.get("BRANCH_ID") != null) {
            branchId = UUID.fromString(params.get("BRANCH_ID").toString());
        }

        LocalDate from = params.containsKey("FROM_DATE")
                ? LocalDate.parse(params.get("FROM_DATE").toString())
                : null;

        LocalDate to = params.containsKey("TO_DATE")
                ? LocalDate.parse(params.get("TO_DATE").toString())
                : null;

        return switch (reportName) {

            case "trial_balance" ->
                    new JRBeanCollectionDataSource(
                            financialStatementService
                                    .getTrialBalanceUniversal(from, to, branchId, mode)
                    );

            case "general_ledger" ->
                    new JRBeanCollectionDataSource(
                            financialStatementService
                                    .getGeneralLedgerUniversal(
                                            UUID.fromString(params.get("ACCOUNT_ID").toString()),
                                            from,
                                            to,
                                            branchId,
                                            mode)
                    );

            case "profit_and_loss" ->
                    new JRBeanCollectionDataSource(
                            financialStatementService
                                    .getProfitAndLossUniversal(from, to, branchId, mode)
                    );

            case "balance_sheet" -> {
                LocalDate asAt =
                        LocalDate.parse(params.get("AS_AT_DATE").toString());

                yield new JRBeanCollectionDataSource(
                        financialStatementService
                                .getBalanceSheetUniversal(asAt, branchId, mode)
                );
            }

            case "cash_flow_statement" ->
                    new JRBeanCollectionDataSource(
                            financialStatementService
                                    .getCashFlowUniversal(from, to, branchId, mode)
                    );

            case "accounts_receivable_summary" ->
                    new JRBeanCollectionDataSource(
                            financialStatementService
                                    .getAccountsReceivableUniversal(from, to, branchId, mode)
                    );

            case "accounts_payable_summary" ->
                    new JRBeanCollectionDataSource(
                            financialStatementService
                                    .getAccountsPayableUniversal(from, to, branchId, mode)
                    );

            default -> throw new IllegalArgumentException("Unsupported financial report");
        };
    }
}