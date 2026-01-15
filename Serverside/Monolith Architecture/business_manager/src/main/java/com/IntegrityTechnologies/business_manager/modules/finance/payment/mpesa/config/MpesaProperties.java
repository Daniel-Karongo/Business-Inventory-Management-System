package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Data
@Component
@ConfigurationProperties(prefix = "mpesa")
public class MpesaProperties {

    /**
     * PAYBILL | TILL
     */
    private String paymentType;

    private String env;
    private String consumerKey;
    private String consumerSecret;
    private String shortcode;
    private String passkey;

    private Stk stk = new Stk();
    private C2b c2b = new C2b();

    @Data
    public static class Stk {
        private String callbackUrl;
    }

    @Data
    public static class C2b {
        private String confirmUrl;
        private String validateUrl;
    }
}