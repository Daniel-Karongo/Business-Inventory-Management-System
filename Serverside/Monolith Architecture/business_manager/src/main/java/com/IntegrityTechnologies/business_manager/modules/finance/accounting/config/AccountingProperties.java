package com.IntegrityTechnologies.business_manager.modules.finance.accounting.config;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountingMode;
import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Getter
@Setter
@Component
@ConfigurationProperties(prefix = "business.accounting")
public class AccountingProperties {

    /**
     * SINGLE_ENTRY | DOUBLE_ENTRY
     */
    private AccountingMode mode = AccountingMode.DOUBLE_ENTRY;
}