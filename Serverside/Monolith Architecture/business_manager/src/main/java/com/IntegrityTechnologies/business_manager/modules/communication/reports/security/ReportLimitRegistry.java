package com.IntegrityTechnologies.business_manager.modules.communication.reports.security;

import java.time.temporal.ChronoUnit;
import java.util.Map;

public class ReportLimitRegistry {

    public static final Map<String, ReportLimits> LIMITS = Map.ofEntries(

            // SALES
            Map.entry("sales_by_product", new ReportLimits(732, ChronoUnit.DAYS)),
            Map.entry("sales_by_branch", new ReportLimits(732, ChronoUnit.DAYS)),
            Map.entry("daily_sales_report", new ReportLimits(31, ChronoUnit.DAYS)),

            // INVENTORY
            Map.entry("inventory_turnover_report", new ReportLimits(732, ChronoUnit.DAYS)),
            Map.entry("inventory_adjustments_report", new ReportLimits(732, ChronoUnit.DAYS)),

            // AUDIT (STRICT)
            Map.entry("user_activity_audit", new ReportLimits(732, ChronoUnit.DAYS)),
            Map.entry("inventory_audit_trail", new ReportLimits(732, ChronoUnit.DAYS)),
            Map.entry("financial_audit_log", new ReportLimits(732, ChronoUnit.DAYS)),
            Map.entry("price_override_audit",new ReportLimits(732, java.time.temporal.ChronoUnit.DAYS)),

            // HR
            Map.entry("attendance_summary",new ReportLimits(732, java.time.temporal.ChronoUnit.DAYS)),

            // PAYMENTS
            Map.entry("mpesa_transactions_report",new ReportLimits(732, java.time.temporal.ChronoUnit.DAYS))

    );

    public static ReportLimits get(String reportName) {
        return LIMITS.get(reportName);
    }
}