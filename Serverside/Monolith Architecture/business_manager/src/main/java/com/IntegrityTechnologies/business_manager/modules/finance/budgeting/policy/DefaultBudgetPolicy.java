package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.policy;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import org.springframework.stereotype.Component;

@Component
public class DefaultBudgetPolicy implements BudgetPolicy {

    @Override
    public void validate(Account account) {

        if (account.getType() == AccountType.ASSET ||
                account.getType() == AccountType.LIABILITY ||
                account.getType() == AccountType.EQUITY) {

            throw new IllegalStateException(
                    "Only income and expense accounts allowed in budgets."
            );
        }
    }
}