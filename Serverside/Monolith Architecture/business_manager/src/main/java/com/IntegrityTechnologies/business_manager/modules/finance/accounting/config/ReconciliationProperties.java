package com.IntegrityTechnologies.business_manager.modules.finance.accounting.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Getter
@Setter
@Component
@ConfigurationProperties(prefix = "accounting.reconciliation")
public class ReconciliationProperties {

    private boolean enabled = true;
    private String cron = "0 0 2 * * ?";
    private boolean autoRepair = false;
}