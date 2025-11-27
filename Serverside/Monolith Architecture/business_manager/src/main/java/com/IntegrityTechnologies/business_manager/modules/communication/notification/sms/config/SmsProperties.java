package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Data
@Component
@ConfigurationProperties(prefix = "sms")
public class SmsProperties {
    private String env;
    private String consumerKey;
    private String consumerSecret;
    private String baseUrlSandbox;
    private String baseUrlProduction;
    private String shortcode;
    private String senderName;
    private String defaultCountryCode;
}