package com.IntegrityTechnologies.business_manager.modules.communication.reports.registry;

import java.util.Map;

public class ReportRegistry {

    public static final Map<String, String> REPORTS = Map.of(
            "trial_balance", "reports/accounting/trial_balance.jrxml",
            "general_ledger", "reports/accounting/general_ledger.jrxml",
            "profit_and_loss", "reports/accounting/profit_and_loss.jrxml",
            "sales_summary", "reports/sales/sales_summary.jrxml"
    );
}