package com.IntegrityTechnologies.business_manager.modules.finance.accounting.config;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountingMode;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.policy.AccountingPolicy;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.policy.DoubleEntryPolicy;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.policy.SingleEntryPolicy;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Map;

@Configuration
public class AccountingPolicyConfiguration {

    @Bean
    public Map<AccountingMode, AccountingPolicy> accountingPolicies(
            DoubleEntryPolicy doubleEntry,
            SingleEntryPolicy singleEntry
    ) {
        return Map.of(
                AccountingMode.DOUBLE_ENTRY, doubleEntry,
                AccountingMode.SINGLE_ENTRY, singleEntry
        );
    }
}