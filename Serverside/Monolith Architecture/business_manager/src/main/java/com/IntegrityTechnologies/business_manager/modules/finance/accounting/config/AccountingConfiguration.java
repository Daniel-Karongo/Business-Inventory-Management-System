package com.IntegrityTechnologies.business_manager.modules.finance.accounting.config;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountingMode;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.policy.*;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@RequiredArgsConstructor
public class AccountingConfiguration {

    private final AccountingProperties properties;

    @Bean
    public AccountingPolicy accountingPolicy() {

        return switch (properties.getMode()) {
            case SINGLE_ENTRY -> new SingleEntryPolicy();
            case DOUBLE_ENTRY -> new DoubleEntryPolicy();
        };
    }
}