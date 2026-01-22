package com.IntegrityTechnologies.business_manager.modules.communication.reports.registry;

import java.util.Map;
import java.util.Set;

public class ReportRegistry {

    public static final Map<String, ReportDefinition> REPORTS = Map.ofEntries(

            // ===================== ACCOUNTING =====================
            Map.entry("trial_balance",
                    new ReportDefinition(
                            "trial_balance",
                            "reports/accounting/trial_balance.jrxml",
                            Set.of("START_DATE", "END_DATE")
                    )
            ),

            Map.entry("general_ledger",
                    new ReportDefinition(
                            "general_ledger",
                            "reports/accounting/general_ledger.jrxml",
                            Set.of("START_DATE", "END_DATE", "ACCOUNT_ID")
                    )
            ),

            Map.entry("profit_and_loss",
                    new ReportDefinition(
                            "profit_and_loss",
                            "reports/accounting/profit_and_loss.jrxml",
                            Set.of("START_DATE", "END_DATE")
                    )
            ),

            Map.entry("balance_sheet",
                    new ReportDefinition(
                            "balance_sheet",
                            "reports/accounting/balance_sheet.jrxml",
                            Set.of("AS_AT_DATE")
                    )
            ),

            Map.entry("cash_flow_statement",
                    new ReportDefinition(
                            "cash_flow_statement",
                            "reports/accounting/cash_flow_statement.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("accounts_receivable_summary",
                    new ReportDefinition(
                            "accounts_receivable_summary",
                            "reports/accounting/accounts_receivable_summary.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("accounts_payable_summary",
                    new ReportDefinition(
                            "accounts_payable_summary",
                            "reports/accounting/accounts_payable_summary.jrxml",
                            Set.of()
                    )
            ),

            // ===================== INVENTORY =====================
            Map.entry("stock_on_hand",
                    new ReportDefinition(
                            "stock_on_hand",
                            "reports/inventory/stock_on_hand.jrxml",
                            Set.of()
                    )
            ),

            Map.entry("inventory_valuation_current",
                    new ReportDefinition(
                            "inventory_valuation_current",
                            "reports/inventory/inventory_valuation_current.jrxml",
                            Set.of()
                    )
            ),

            Map.entry("inventory_valuation_historical",
                    new ReportDefinition(
                            "inventory_valuation_historical",
                            "reports/inventory/inventory_valuation_historical.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("stock_movement_ledger",
                    new ReportDefinition(
                            "stock_movement_ledger",
                            "reports/inventory/stock_movement_ledger.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("inventory_stock_movement",
                    new ReportDefinition(
                            "inventory_stock_movement",
                            "reports/inventory/inventory_stock_movement.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("inventory_aging",
                    new ReportDefinition(
                            "inventory_aging",
                            "reports/inventory/inventory_aging.jrxml",
                            Set.of("AS_AT_DATE")
                    )
            ),

            Map.entry("low_stock_report",
                    new ReportDefinition(
                            "low_stock_report",
                            "reports/inventory/low_stock_report.jrxml",
                            Set.of("THRESHOLD")
                    )
            ),

            Map.entry("out_of_stock_report",
                    new ReportDefinition(
                            "out_of_stock_report",
                            "reports/inventory/out_of_stock_report.jrxml",
                            Set.of()
                    )
            ),

            Map.entry("fast_moving_items",
                    new ReportDefinition(
                            "fast_moving_items",
                            "reports/inventory/fast_moving_items.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("slow_moving_items",
                    new ReportDefinition(
                            "slow_moving_items",
                            "reports/inventory/slow_moving_items.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("dead_stock_report",
                    new ReportDefinition(
                            "dead_stock_report",
                            "reports/inventory/dead_stock_report.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("inventory_adjustments_report",
                    new ReportDefinition(
                            "inventory_adjustments_report",
                            "reports/inventory/inventory_adjustments_report.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("inventory_turnover_report",
                    new ReportDefinition(
                            "inventory_turnover_report",
                            "reports/inventory/inventory_turnover_report.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            // ===================== SALES =====================
            Map.entry("sales_by_product",
                    new ReportDefinition(
                            "sales_by_product",
                            "reports/sales/sales_by_product.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("sales_by_branch",
                    new ReportDefinition(
                            "sales_by_branch",
                            "reports/sales/sales_by_branch.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("sales_summary",
                    new ReportDefinition(
                            "sales_summary",
                            "reports/sales/sales_summary.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("sales_by_category",
                    new ReportDefinition(
                            "sales_by_category",
                            "reports/sales/sales_by_category.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("sales_by_customer",
                    new ReportDefinition(
                            "sales_by_customer",
                            "reports/sales/sales_by_customer.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("daily_sales_report",
                    new ReportDefinition(
                            "daily_sales_report",
                            "reports/sales/daily_sales_report.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("monthly_sales_report",
                    new ReportDefinition(
                            "monthly_sales_report",
                            "reports/sales/monthly_sales_report.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("sales_refunds_report",
                    new ReportDefinition(
                            "sales_refunds_report",
                            "reports/sales/sales_refunds_report.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("single_day_sales_report",
                    new ReportDefinition(
                            "single_day_sales_report",
                            "reports/sales/single_day_sales_report.jrxml",
                            Set.of("SALE_DATE")
                    )
            ),

            // ===================== CUSTOMERS =====================
            Map.entry("customer_list",
                    new ReportDefinition(
                            "customer_list",
                            "reports/customers/customer_list.jrxml",
                            Set.of()
                    )
            ),

            Map.entry("customer_sales_history",
                    new ReportDefinition(
                            "customer_sales_history",
                            "reports/customers/customer_sales_history.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("customer_payment_history",
                    new ReportDefinition(
                            "customer_payment_history",
                            "reports/customers/customer_payment_history.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("customer_sales_history_single",
                    new ReportDefinition(
                            "customer_sales_history_single",
                            "reports/customers/customer_sales_history_single.jrxml",
                            Set.of("CUSTOMER_ID")
                    )
            ),

            Map.entry("customer_outstanding_balance_single",
                    new ReportDefinition(
                            "customer_outstanding_balance_single",
                            "reports/customers/customer_outstanding_balance_single.jrxml",
                            Set.of("CUSTOMER_ID")
                    )
            ),

            Map.entry("customer_refunds_single",
                    new ReportDefinition(
                            "customer_refunds_single",
                            "reports/customers/customer_refunds_single.jrxml",
                            Set.of("CUSTOMER_ID")
                    )
            ),

            Map.entry("customer_activity_timeline_single",
                    new ReportDefinition(
                            "customer_activity_timeline_single",
                            "reports/customers/customer_activity_timeline_single.jrxml",
                            Set.of("CUSTOMER_ID")
                    )
            ),

            Map.entry("customer_activity_summary",
                    new ReportDefinition(
                            "customer_activity_summary",
                            "reports/customers/customer_activity_summary.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("customer_activity_timeline_all",
                    new ReportDefinition(
                            "customer_activity_timeline_all",
                            "reports/customers/customer_activity_timeline_all.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("top_customers_by_revenue",
                    new ReportDefinition(
                            "top_customers_by_revenue",
                            "reports/customers/top_customers_by_revenue.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("inactive_customers",
                    new ReportDefinition(
                            "inactive_customers",
                            "reports/customers/inactive_customers.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            // ===================== SUPPLIERS =====================
            Map.entry("supplier_list",
                    new ReportDefinition(
                            "supplier_list",
                            "reports/suppliers/supplier_list.jrxml",
                            Set.of()
                    )
            ),

            Map.entry("supplier_items_supplied_single",
                    new ReportDefinition(
                            "supplier_items_supplied_single",
                            "reports/suppliers/supplier_items_supplied_single.jrxml",
                            Set.of("SUPPLIER_ID", "FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("supplier_items_supplied_all",
                    new ReportDefinition(
                            "supplier_items_supplied_all",
                            "reports/suppliers/supplier_items_supplied_all.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("supplier_price_variance",
                    new ReportDefinition(
                            "supplier_price_variance",
                            "reports/suppliers/supplier_price_variance.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("supplier_price_variance_by_product",
                    new ReportDefinition(
                            "supplier_price_variance_by_product",
                            "reports/suppliers/supplier_price_variance_by_product.jrxml",
                            Set.of("PRODUCT_ID", "FROM_DATE", "TO_DATE")
                    )
            ),

            // ===================== PAYMENTS =====================
            Map.entry("payments_summary",
                    new ReportDefinition(
                            "payments_summary",
                            "reports/payments/payments_summary.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("payments_by_method",
                    new ReportDefinition(
                            "payments_by_method",
                            "reports/payments/payments_by_method.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("daily_payments_report",
                    new ReportDefinition(
                            "daily_payments_report",
                            "reports/payments/daily_payments_report.jrxml",
                            Set.of("REPORT_DATE")
                    )
            ),

            // ===================== AUDIT =====================
            Map.entry("user_activity_audit",
                    new ReportDefinition(
                            "user_activity_audit",
                            "reports/audit/user_activity_audit.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("inventory_audit_trail",
                    new ReportDefinition(
                            "inventory_audit_trail",
                            "reports/audit/inventory_audit_trail.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            ),

            Map.entry("financial_audit_log",
                    new ReportDefinition(
                            "financial_audit_log",
                            "reports/audit/financial_audit_log.jrxml",
                            Set.of("FROM_DATE", "TO_DATE")
                    )
            )
    );

    public static ReportDefinition get(String key) {
        return REPORTS.get(key);
    }
}