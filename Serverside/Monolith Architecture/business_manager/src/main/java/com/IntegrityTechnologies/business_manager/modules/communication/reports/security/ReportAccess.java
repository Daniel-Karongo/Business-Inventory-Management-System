package com.IntegrityTechnologies.business_manager.modules.communication.reports.security;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;

public enum ReportAccess {

    /* ===================== SALES ===================== */
    SALES_BY_PRODUCT("sales_by_product", Role.MANAGER),
    SALES_BY_BRANCH("sales_by_branch", Role.MANAGER),
    SALES_BY_CATEGORY("sales_by_category", Role.MANAGER),
    SALES_BY_CUSTOMER("sales_by_customer", Role.MANAGER),
    SALES_SUMMARY("sales_summary", Role.MANAGER),
    DAILY_SALES_REPORT("daily_sales_report", Role.SUPERVISOR),
    MONTHLY_SALES_REPORT("monthly_sales_report", Role.MANAGER),
    SALES_REFUNDS_REPORT("sales_refunds_report", Role.MANAGER),
    SINGLE_DAY_SALES_REPORT("single_day_sales_report", Role.SUPERVISOR),

    /* ===================== CUSTOMERS ===================== */
    CUSTOMER_LIST("customer_list", Role.SUPERVISOR),
    CUSTOMER_SALES_HISTORY("customer_sales_history", Role.SUPERVISOR),
    CUSTOMER_PAYMENT_HISTORY("customer_payment_history", Role.SUPERVISOR),

    CUSTOMER_OUTSTANDING_BALANCES(
            "customer_outstanding_balances", Role.MANAGER
    ),

    CUSTOMER_SALES_HISTORY_SINGLE(
            "customer_sales_history_single", Role.SUPERVISOR
    ),
    CUSTOMER_OUTSTANDING_BALANCE_SINGLE(
            "customer_outstanding_balance_single", Role.SUPERVISOR
    ),
    CUSTOMER_REFUNDS_SINGLE(
            "customer_refunds_single", Role.SUPERVISOR
    ),
    CUSTOMER_ACTIVITY_TIMELINE_SINGLE(
            "customer_activity_timeline_single", Role.SUPERVISOR
    ),

    CUSTOMER_ACTIVITY_SUMMARY(
            "customer_activity_summary", Role.MANAGER
    ),
    CUSTOMER_ACTIVITY_TIMELINE_ALL(
            "customer_activity_timeline_all", Role.MANAGER
    ),
    TOP_CUSTOMERS_BY_REVENUE(
            "top_customers_by_revenue", Role.MANAGER
    ),
    INACTIVE_CUSTOMERS(
            "inactive_customers", Role.MANAGER
    ),

    /* ===================== ACCOUNTING ===================== */
    TRIAL_BALANCE("trial_balance", Role.ADMIN),
    GENERAL_LEDGER("general_ledger", Role.ADMIN),
    PROFIT_AND_LOSS("profit_and_loss", Role.ADMIN),
    BALANCE_SHEET("balance_sheet", Role.ADMIN),
    CASH_FLOW_STATEMENT("cash_flow_statement", Role.ADMIN),
    ACCOUNTS_RECEIVABLE_SUMMARY(
            "accounts_receivable_summary", Role.ADMIN
    ),
    ACCOUNTS_PAYABLE_SUMMARY(
            "accounts_payable_summary", Role.ADMIN
    ),

    /* ===================== INVENTORY ===================== */
    STOCK_ON_HAND("stock_on_hand", Role.SUPERVISOR),
    INVENTORY_VALUATION_CURRENT(
            "inventory_valuation_current", Role.MANAGER
    ),
    INVENTORY_VALUATION_HISTORICAL(
            "inventory_valuation_historical", Role.MANAGER
    ),
    STOCK_MOVEMENT_LEDGER(
            "stock_movement_ledger", Role.MANAGER
    ),
    INVENTORY_STOCK_MOVEMENT(
            "inventory_stock_movement", Role.MANAGER
    ),
    INVENTORY_AGING("inventory_aging", Role.MANAGER),
    LOW_STOCK_REPORT("low_stock_report", Role.SUPERVISOR),
    OUT_OF_STOCK_REPORT("out_of_stock_report", Role.SUPERVISOR),
    FAST_MOVING_ITEMS("fast_moving_items", Role.MANAGER),
    SLOW_MOVING_ITEMS("slow_moving_items", Role.MANAGER),
    DEAD_STOCK_REPORT("dead_stock_report", Role.MANAGER),
    INVENTORY_ADJUSTMENTS_REPORT(
            "inventory_adjustments_report", Role.MANAGER
    ),
    INVENTORY_TURNOVER_REPORT(
            "inventory_turnover_report", Role.MANAGER
    ),

    /* ===================== SUPPLIERS ===================== */
    SUPPLIER_LIST("supplier_list", Role.SUPERVISOR),
    SUPPLIER_ITEMS_SUPPLIED_SINGLE(
            "supplier_items_supplied_single", Role.MANAGER
    ),
    SUPPLIER_ITEMS_SUPPLIED_ALL(
            "supplier_items_supplied_all", Role.MANAGER
    ),
    SUPPLIER_PRICE_VARIANCE(
            "supplier_price_variance", Role.ADMIN
    ),
    SUPPLIER_PRICE_VARIANCE_BY_PRODUCT(
            "supplier_price_variance_by_product", Role.ADMIN
    ),

    /* ===================== PAYMENTS ===================== */
    PAYMENTS_SUMMARY("payments_summary", Role.ADMIN),
    PAYMENTS_BY_METHOD("payments_by_method", Role.ADMIN),
    DAILY_PAYMENTS_REPORT(
            "daily_payments_report", Role.SUPERVISOR
    ),

    MPESA_TRANSACTIONS_REPORT(
            "mpesa_transactions_report", Role.ADMIN
    ),

    /* ===================== AUDIT ===================== */
    USER_ACTIVITY_AUDIT("user_activity_audit", Role.ADMIN),
    INVENTORY_AUDIT_TRAIL("inventory_audit_trail", Role.ADMIN),
    FINANCIAL_AUDIT_LOG("financial_audit_log", Role.SUPERUSER),

    PRICE_OVERRIDE_AUDIT(
            "price_override_audit", Role.SUPERUSER
    ),

    /* ===================== HR ===================== */
    ATTENDANCE_SUMMARY(
            "attendance_summary", Role.MANAGER
    );

    /* ===================== INTERNAL ===================== */

    private final String reportName;
    private final Role minimumRole;

    ReportAccess(String reportName, Role minimumRole) {
        this.reportName = reportName;
        this.minimumRole = minimumRole;
    }

    public String getReportName() {
        return reportName;
    }

    public Role getMinimumRole() {
        return minimumRole;
    }

    public boolean canAccess(Role userRole) {
        return userRole.canAccess(minimumRole);
    }

    public static ReportAccess fromReportName(String reportName) {
        for (ReportAccess access : values()) {
            if (access.reportName.equals(reportName)) {
                return access;
            }
        }
        throw new IllegalArgumentException(
                "No access rule defined for report: " + reportName
        );
    }
}