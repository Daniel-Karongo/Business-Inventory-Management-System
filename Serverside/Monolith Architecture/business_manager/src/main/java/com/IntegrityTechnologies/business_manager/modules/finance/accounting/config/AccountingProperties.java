package com.IntegrityTechnologies.business_manager.modules.finance.accounting.config;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.RevenueRecognitionMode;
import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Data
@Component
@ConfigurationProperties(prefix = "business.accounting")
public class AccountingProperties {

    private String revenueRecognition = "DELIVERY";

    public RevenueRecognitionMode getRevenueRecognitionMode() {
        return RevenueRecognitionMode.valueOf(
                revenueRecognition.toUpperCase()
        );
    }
}