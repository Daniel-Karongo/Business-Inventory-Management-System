package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.provider;

public interface SmsProvider {
    SmsProviderResponse send(
            String to,
            String message,
            SmsAccount account
    );
}