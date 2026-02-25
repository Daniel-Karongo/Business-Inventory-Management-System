package com.IntegrityTechnologies.business_manager.modules.communication.reports.registry;

import java.util.Map;
import java.util.Set;

public class ReportRegistry {

        public static final Map<String, ReportDefinition> REPORTS = Map.ofEntries(

                /* ===================== ACCOUNTING ===================== */

                Map.entry("trial_balance",
                        new ReportDefinition(
                                "trial_balance",
                                "Trial Balance",
                                "Balances by account over a period",
                                "Accounting",
                                "reports/accounting/trial_balance.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("trial_balance_multi_branch",
                        new ReportDefinition(
                                "trial_balance_multi_branch",
                                "Trial Balance (Multi-Branch)",
                                "Trial balance grouped by branch with consolidated totals",
                                "Accounting",
                                "reports/accounting/trial_balance_multi_branch.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("general_ledger",
                        new ReportDefinition(
                                "general_ledger",
                                "General Ledger",
                                "Detailed account ledger",
                                "Accounting",
                                "reports/accounting/general_ledger.jrxml",
                                Set.of("FROM_DATE", "TO_DATE", "ACCOUNT_ID")
                        )
                ),

                Map.entry("general_ledger_multi_branch",
                        new ReportDefinition(
                                "general_ledger_multi_branch",
                                "General Ledger (Multi-Branch)",
                                "Detailed account ledger grouped by branch",
                                "Accounting",
                                "reports/accounting/general_ledger_multi_branch.jrxml",
                                Set.of("FROM_DATE", "TO_DATE", "ACCOUNT_ID")
                        )
                ),

                Map.entry("profit_and_loss",
                        new ReportDefinition(
                                "profit_and_loss",
                                "Profit & Loss",
                                "Income versus expenses",
                                "Accounting",
                                "reports/accounting/profit_and_loss.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("profit_and_loss_multi_branch",
                        new ReportDefinition(
                                "profit_and_loss_multi_branch",
                                "Profit & Loss (Multi-Branch)",
                                "Income versus expenses grouped by branch",
                                "Accounting",
                                "reports/accounting/profit_and_loss_multi_branch.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("balance_sheet",
                        new ReportDefinition(
                                "balance_sheet",
                                "Balance Sheet",
                                "Assets, liabilities and equity snapshot",
                                "Accounting",
                                "reports/accounting/balance_sheet.jrxml",
                                Set.of("AS_AT_DATE")
                        )
                ),

                Map.entry("balance_sheet_multi_branch",
                        new ReportDefinition(
                                "balance_sheet_multi_branch",
                                "Balance Sheet (Multi-Branch)",
                                "Balance sheet grouped by branch",
                                "Accounting",
                                "reports/accounting/balance_sheet_multi_branch.jrxml",
                                Set.of("AS_AT_DATE")
                        )
                ),

                Map.entry("cash_flow_statement",
                        new ReportDefinition(
                                "cash_flow_statement",
                                "Cash Flow Statement",
                                "Cash inflows and outflows",
                                "Accounting",
                                "reports/accounting/cash_flow_statement.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("cash_flow_statement_multi_branch",
                        new ReportDefinition(
                                "cash_flow_statement_multi_branch",
                                "Cash Flow Statement (Multi-Branch)",
                                "Cash inflows and outflows grouped by branch",
                                "Accounting",
                                "reports/accounting/cash_flow_statement_multi_branch.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("accounts_receivable_summary",
                        new ReportDefinition(
                                "accounts_receivable_summary",
                                "Accounts Receivable Summary",
                                "Outstanding customer balances",
                                "Accounting",
                                "reports/accounting/accounts_receivable_summary.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("accounts_receivable_summary_multi_branch",
                        new ReportDefinition(
                                "accounts_receivable_summary_multi_branch",
                                "Accounts Receivable (Multi-Branch)",
                                "Outstanding customer balances grouped by branch",
                                "Accounting",
                                "reports/accounting/accounts_receivable_summary_multi_branch.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("accounts_payable_summary",
                        new ReportDefinition(
                                "accounts_payable_summary",
                                "Accounts Payable Summary",
                                "Outstanding supplier balances",
                                "Accounting",
                                "reports/accounting/accounts_payable_summary.jrxml",
                                Set.of()
                        )
                ),

                Map.entry("accounts_payable_summary_multi_branch",
                        new ReportDefinition(
                                "accounts_payable_summary_multi_branch",
                                "Accounts Payable (Multi-Branch)",
                                "Outstanding supplier balances grouped by branch",
                                "Accounting",
                                "reports/accounting/accounts_payable_summary_multi_branch.jrxml",
                                Set.of()
                        )
                ),

                /* ===================== INVENTORY ===================== */

                Map.entry("stock_on_hand",
                        new ReportDefinition(
                                "stock_on_hand",
                                "Stock on Hand",
                                "Current inventory quantities",
                                "Inventory",
                                "reports/inventory/stock_on_hand.jrxml",
                                Set.of()
                        )
                ),

                Map.entry("inventory_valuation_current",
                        new ReportDefinition(
                                "inventory_valuation_current",
                                "Inventory Valuation (Current)",
                                "Current inventory value",
                                "Inventory",
                                "reports/inventory/inventory_valuation_current.jrxml",
                                Set.of()
                        )
                ),

                Map.entry("inventory_valuation_historical",
                        new ReportDefinition(
                                "inventory_valuation_historical",
                                "Inventory Valuation (Historical)",
                                "Inventory value over time",
                                "Inventory",
                                "reports/inventory/inventory_valuation_historical.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("stock_movement_ledger",
                        new ReportDefinition(
                                "stock_movement_ledger",
                                "Stock Movement Ledger",
                                "Chronological inventory movements",
                                "Inventory",
                                "reports/inventory/stock_movement_ledger.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("inventory_stock_movement",
                        new ReportDefinition(
                                "inventory_stock_movement",
                                "Inventory Stock Movement",
                                "Detailed stock movement by branch",
                                "Inventory",
                                "reports/inventory/inventory_stock_movement.jrxml",
                                Set.of("BRANCH_ID", "FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("inventory_aging",
                        new ReportDefinition(
                                "inventory_aging",
                                "Inventory Aging",
                                "Age of inventory items",
                                "Inventory",
                                "reports/inventory/inventory_aging.jrxml",
                                Set.of("AS_AT_DATE")
                        )
                ),

                Map.entry("low_stock_report",
                        new ReportDefinition(
                                "low_stock_report",
                                "Low Stock Report",
                                "Items below reorder level",
                                "Inventory",
                                "reports/inventory/low_stock_report.jrxml",
                                Set.of("THRESHOLD")
                        )
                ),

                Map.entry("out_of_stock_report",
                        new ReportDefinition(
                                "out_of_stock_report",
                                "Out of Stock Report",
                                "Items currently out of stock",
                                "Inventory",
                                "reports/inventory/out_of_stock_report.jrxml",
                                Set.of()
                        )
                ),

                Map.entry("fast_moving_items",
                        new ReportDefinition(
                                "fast_moving_items",
                                "Fast Moving Items",
                                "High turnover inventory",
                                "Inventory",
                                "reports/inventory/fast_moving_items.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("slow_moving_items",
                        new ReportDefinition(
                                "slow_moving_items",
                                "Slow Moving Items",
                                "Low turnover inventory",
                                "Inventory",
                                "reports/inventory/slow_moving_items.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("dead_stock_report",
                        new ReportDefinition(
                                "dead_stock_report",
                                "Dead Stock Report",
                                "Inventory with no movement",
                                "Inventory",
                                "reports/inventory/dead_stock_report.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("inventory_adjustments_report",
                        new ReportDefinition(
                                "inventory_adjustments_report",
                                "Inventory Adjustments",
                                "Manual stock adjustments",
                                "Inventory",
                                "reports/inventory/inventory_adjustments_report.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("inventory_turnover_report",
                        new ReportDefinition(
                                "inventory_turnover_report",
                                "Inventory Turnover",
                                "Inventory turnover ratio",
                                "Inventory",
                                "reports/inventory/inventory_turnover_report.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                /* ===================== SALES ===================== */

                Map.entry("sales_summary",
                        new ReportDefinition(
                                "sales_summary",
                                "Sales Summary",
                                "Overall sales performance",
                                "Sales",
                                "reports/sales/sales_summary.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("sales_by_product",
                        new ReportDefinition(
                                "sales_by_product",
                                "Sales by Product",
                                "Sales grouped by product",
                                "Sales",
                                "reports/sales/sales_by_product.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("sales_by_branch",
                        new ReportDefinition(
                                "sales_by_branch",
                                "Sales by Branch",
                                "Sales grouped by branch",
                                "Sales",
                                "reports/sales/sales_by_branch.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("sales_by_category",
                        new ReportDefinition(
                                "sales_by_category",
                                "Sales by Category",
                                "Sales grouped by product category",
                                "Sales",
                                "reports/sales/sales_by_category.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("sales_by_customer",
                        new ReportDefinition(
                                "sales_by_customer",
                                "Sales by Customer",
                                "Customer sales totals",
                                "Sales",
                                "reports/sales/sales_by_customer.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("daily_sales_report",
                        new ReportDefinition(
                                "daily_sales_report",
                                "Daily Sales Report",
                                "Sales for a specific day",
                                "Sales",
                                "reports/sales/daily_sales_report.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("monthly_sales_report",
                        new ReportDefinition(
                                "monthly_sales_report",
                                "Monthly Sales Report",
                                "Sales grouped by month",
                                "Sales",
                                "reports/sales/monthly_sales_report.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("sales_refunds_report",
                        new ReportDefinition(
                                "sales_refunds_report",
                                "Sales Refunds Report",
                                "Refunded sales",
                                "Sales",
                                "reports/sales/sales_refunds_report.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("single_day_sales_report",
                        new ReportDefinition(
                                "single_day_sales_report",
                                "Single Day Sales Report",
                                "Sales for a single day",
                                "Sales",
                                "reports/sales/single_day_sales_report.jrxml",
                                Set.of("SALE_DATE")
                        )
                ),

                /* ===================== CUSTOMERS ===================== */

                Map.entry("customer_list",
                        new ReportDefinition(
                                "customer_list",
                                "Customer List",
                                "All registered customers",
                                "Customers",
                                "reports/customers/customer_list.jrxml",
                                Set.of()
                        )
                ),

                Map.entry("customer_sales_history",
                        new ReportDefinition(
                                "customer_sales_history",
                                "Customer Sales History",
                                "Sales history for all customers",
                                "Customers",
                                "reports/customers/customer_sales_history.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("customer_payment_history",
                        new ReportDefinition(
                                "customer_payment_history",
                                "Customer Payment History",
                                "Customer payments over time",
                                "Customers",
                                "reports/customers/customer_payment_history.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("customer_sales_history_single",
                        new ReportDefinition(
                                "customer_sales_history_single",
                                "Customer Sales History (Single)",
                                "Sales history for a specific customer",
                                "Customers",
                                "reports/customers/customer_sales_history_single.jrxml",
                                Set.of("CUSTOMER_ID")
                        )
                ),

                Map.entry("customer_outstanding_balance_single",
                        new ReportDefinition(
                                "customer_outstanding_balance_single",
                                "Customer Outstanding Balance",
                                "Outstanding balance for a customer",
                                "Customers",
                                "reports/customers/customer_outstanding_balance_single.jrxml",
                                Set.of("CUSTOMER_ID")
                        )
                ),

                Map.entry("customer_outstanding_balances",
                        new ReportDefinition(
                                "customer_outstanding_balances",
                                "Customer Outstanding Balances",
                                "Outstanding balances for all customers",
                                "Customers",
                                "reports/customers/customer_outstanding_balances.jrxml",
                                Set.of()
                        )
                ),

                Map.entry("customer_refunds_single",
                        new ReportDefinition(
                                "customer_refunds_single",
                                "Customer Refunds",
                                "Refunds issued to a customer",
                                "Customers",
                                "reports/customers/customer_refunds_single.jrxml",
                                Set.of("CUSTOMER_ID")
                        )
                ),

                Map.entry("customer_activity_timeline_single",
                        new ReportDefinition(
                                "customer_activity_timeline_single",
                                "Customer Activity Timeline",
                                "Activity timeline for a customer",
                                "Customers",
                                "reports/customers/customer_activity_timeline_single.jrxml",
                                Set.of("CUSTOMER_ID")
                        )
                ),

                Map.entry("customer_activity_summary",
                        new ReportDefinition(
                                "customer_activity_summary",
                                "Customer Activity Summary",
                                "Customer activity overview",
                                "Customers",
                                "reports/customers/customer_activity_summary.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("customer_activity_timeline_all",
                        new ReportDefinition(
                                "customer_activity_timeline_all",
                                "Customer Activity Timeline (All)",
                                "Activity timeline for all customers",
                                "Customers",
                                "reports/customers/customer_activity_timeline_all.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("top_customers_by_revenue",
                        new ReportDefinition(
                                "top_customers_by_revenue",
                                "Top Customers by Revenue",
                                "Highest revenue customers",
                                "Customers",
                                "reports/customers/top_customers_by_revenue.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("inactive_customers",
                        new ReportDefinition(
                                "inactive_customers",
                                "Inactive Customers",
                                "Customers with no recent activity",
                                "Customers",
                                "reports/customers/inactive_customers.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                /* ===================== SUPPLIERS ===================== */

                Map.entry("supplier_list",
                        new ReportDefinition(
                                "supplier_list",
                                "Supplier List",
                                "All registered suppliers",
                                "Suppliers",
                                "reports/suppliers/supplier_list.jrxml",
                                Set.of()
                        )
                ),

                Map.entry("supplier_items_supplied_single",
                        new ReportDefinition(
                                "supplier_items_supplied_single",
                                "Supplier Items Supplied",
                                "Items supplied by a supplier",
                                "Suppliers",
                                "reports/suppliers/supplier_items_supplied_single.jrxml",
                                Set.of("SUPPLIER_ID", "FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("supplier_items_supplied_all",
                        new ReportDefinition(
                                "supplier_items_supplied_all",
                                "Supplier Items Supplied (All)",
                                "Items supplied by all suppliers",
                                "Suppliers",
                                "reports/suppliers/supplier_items_supplied_all.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("supplier_price_variance",
                        new ReportDefinition(
                                "supplier_price_variance",
                                "Supplier Price Variance",
                                "Supplier price comparison",
                                "Suppliers",
                                "reports/suppliers/supplier_price_variance.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("supplier_price_variance_by_product",
                        new ReportDefinition(
                                "supplier_price_variance_by_product",
                                "Supplier Price Variance by Product",
                                "Supplier pricing per product",
                                "Suppliers",
                                "reports/suppliers/supplier_price_variance_by_product.jrxml",
                                Set.of("PRODUCT_ID", "FROM_DATE", "TO_DATE")
                        )
                ),

                /* ===================== PAYMENTS ===================== */

                Map.entry("payments_summary",
                        new ReportDefinition(
                                "payments_summary",
                                "Payments Summary",
                                "All payments summary",
                                "Payments",
                                "reports/payments/payments_summary.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("payments_by_method",
                        new ReportDefinition(
                                "payments_by_method",
                                "Payments by Method",
                                "Payments grouped by method",
                                "Payments",
                                "reports/payments/payments_by_method.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("daily_payments_report",
                        new ReportDefinition(
                                "daily_payments_report",
                                "Daily Payments Report",
                                "Payments for a specific day",
                                "Payments",
                                "reports/payments/daily_payments_report.jrxml",
                                Set.of("REPORT_DATE")
                        )
                ),

                /* ===================== AUDIT ===================== */

                Map.entry("attendance_summary",
                        new ReportDefinition(
                                "attendance_summary",
                                "Attendance Summary",
                                "Employee attendance overview",
                                "HR",
                                "reports/hr/attendance_summary.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("user_activity_audit",
                        new ReportDefinition(
                                "user_activity_audit",
                                "User Activity Audit",
                                "User actions audit log",
                                "Audit",
                                "reports/audit/user_activity_audit.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("inventory_audit_trail",
                        new ReportDefinition(
                                "inventory_audit_trail",
                                "Inventory Audit Trail",
                                "Inventory change audit log",
                                "Audit",
                                "reports/audit/inventory_audit_trail.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("price_override_audit",
                        new ReportDefinition(
                                "price_override_audit",
                                "Price Override Audit",
                                "Audit log of overridden prices",
                                "Audit",
                                "reports/audit/price_override_audit.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                ),

                Map.entry("financial_audit_log",
                        new ReportDefinition(
                                "financial_audit_log",
                                "Financial Audit Log",
                                "Financial operations audit log",
                                "Audit",
                                "reports/audit/financial_audit_log.jrxml",
                                Set.of("FROM_DATE", "TO_DATE")
                        )
                )
        );

        public static ReportDefinition get(String key) {
                return REPORTS.get(key);
        }
}