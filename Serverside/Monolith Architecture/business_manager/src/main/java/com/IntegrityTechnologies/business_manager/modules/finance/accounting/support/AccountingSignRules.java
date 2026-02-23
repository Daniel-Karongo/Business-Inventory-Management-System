package com.IntegrityTechnologies.business_manager.modules.finance.accounting.support;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;

import java.util.Set;

public final class AccountingSignRules {

    private AccountingSignRules() {}

    /* ===================== NORMAL BALANCE TYPES ===================== */

    public static final Set<AccountType> DEBIT_NORMAL =
            Set.of(AccountType.ASSET, AccountType.EXPENSE);

    public static final Set<AccountType> CREDIT_NORMAL =
            Set.of(AccountType.LIABILITY, AccountType.EQUITY, AccountType.INCOME);

    /* ===================== DIRECTIONS ===================== */

    public static final EntryDirection DEBIT = EntryDirection.DEBIT;
    public static final EntryDirection CREDIT = EntryDirection.CREDIT;
}