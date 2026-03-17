package com.IntegrityTechnologies.business_manager.modules.finance.accounting.cache;

import com.IntegrityTechnologies.business_manager.security.util.TenantContext;

import java.time.LocalDate;
import java.util.UUID;

public final class AccountingCacheKey {

    private AccountingCacheKey(){}

    private static UUID tenant() {
        return TenantContext.getTenantId();
    }

    /* =========================================================
       TRIAL BALANCE
    ========================================================= */

    public static String trialBalance(UUID branchId, LocalDate from, LocalDate to) {
        return tenant() + "::trialBalance::" + branchId + "::" + from + "::" + to;
    }

    public static String trialBalanceMulti(LocalDate from, LocalDate to) {
        return tenant() + "::trialBalance::MULTI::" + from + "::" + to;
    }

    /* =========================================================
       PROFIT & LOSS
    ========================================================= */

    public static String profitLoss(UUID branchId, LocalDate from, LocalDate to) {
        return tenant() + "::profitLoss::" + branchId + "::" + from + "::" + to;
    }

    public static String profitLossMulti(LocalDate from, LocalDate to) {
        return tenant() + "::profitLoss::MULTI::" + from + "::" + to;
    }

    /* =========================================================
       BALANCE SHEET
    ========================================================= */

    public static String balanceSheet(UUID branchId, LocalDate asAt) {
        return tenant() + "::balanceSheet::" + branchId + "::" + asAt;
    }

    public static String balanceSheetMulti(LocalDate asAt) {
        return tenant() + "::balanceSheet::MULTI::" + asAt;
    }

    /* =========================================================
       CASH FLOW
    ========================================================= */

    public static String cashFlow(UUID branchId, LocalDate from, LocalDate to) {
        return tenant() + "::cashFlow::" + branchId + "::" + from + "::" + to;
    }

    public static String cashFlowMulti(LocalDate from, LocalDate to) {
        return tenant() + "::cashFlow::MULTI::" + from + "::" + to;
    }

    /* =========================================================
       RECEIVABLES
    ========================================================= */

    public static String receivables(UUID branchId, LocalDate from, LocalDate to) {
        return tenant() + "::receivables::" + branchId + "::" + from + "::" + to;
    }

    public static String receivablesMulti(LocalDate from, LocalDate to) {
        return tenant() + "::receivables::MULTI::" + from + "::" + to;
    }

    /* =========================================================
       PAYABLES
    ========================================================= */

    public static String payables(UUID branchId, LocalDate from, LocalDate to) {
        return tenant() + "::payables::" + branchId + "::" + from + "::" + to;
    }

    public static String payablesMulti(LocalDate from, LocalDate to) {
        return tenant() + "::payables::MULTI::" + from + "::" + to;
    }

}