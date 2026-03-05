package com.IntegrityTechnologies.business_manager.modules.finance.accounting.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Getter
@Setter
@Component
@ConfigurationProperties(prefix = "accounting.integrity")
public class IntegrityProperties {

    private boolean enabled = true;

    /**
     * Default: 2:00 AM nightly
     */
    private String cron = "0 0 2 * * ?";
}