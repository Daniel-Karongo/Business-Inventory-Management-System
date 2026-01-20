package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Data
@Component
@ConfigurationProperties(prefix = "sms")
public class SmsProperties {

    private String provider;
    private String env;
    private String defaultCountryCode;

    private Africa africa;

    @Data
    public static class Africa {
        private String username;
        private String apiKey;
        private String senderId;
        private String baseUrlSandbox;
        private String baseUrlProduction;
    }
}