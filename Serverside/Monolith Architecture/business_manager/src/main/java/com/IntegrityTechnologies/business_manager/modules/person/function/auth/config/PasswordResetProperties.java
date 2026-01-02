package com.IntegrityTechnologies.business_manager.modules.person.function.auth.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.util.List;

@Data
@Component
@ConfigurationProperties(prefix = "auth.password-reset")
public class PasswordResetProperties {

    private boolean enabled;

    private List<Channel> channels;

    private Email email = new Email();
    private Sms sms = new Sms();
    private Identity identity = new Identity();

    private int maxAttempts = 5;
    private boolean invalidateSessions = true;

    public enum Channel {
        EMAIL, SMS, IDENTITY
    }

    @Data
    public static class Email {
        private boolean enabled;
        private int tokenExpiryMinutes = 15;
    }

    @Data
    public static class Sms {
        private boolean enabled;
        private int tokenExpiryMinutes = 10;
    }

    @Data
    public static class Identity {
        private boolean enabled;
        private List<String> requiredFields;
    }
}