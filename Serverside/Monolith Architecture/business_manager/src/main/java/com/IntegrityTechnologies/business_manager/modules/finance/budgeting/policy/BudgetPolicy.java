package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.policy;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;

public interface BudgetPolicy {

    void validate(Account account);
}