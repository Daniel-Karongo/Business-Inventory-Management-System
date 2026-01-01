package com.IntegrityTechnologies.business_manager.modules.finance.accounting.config;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.policy.*;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class AccountingConfiguration {

    @Value("${accounting.mode:DOUBLE_ENTRY}")
    private String accountingMode;

    @Bean
    public AccountingPolicy accountingPolicy() {
        return switch (accountingMode.toUpperCase()) {
            case "SINGLE_ENTRY" -> new SingleEntryPolicy();
            default -> new DoubleEntryPolicy();
        };
    }
}