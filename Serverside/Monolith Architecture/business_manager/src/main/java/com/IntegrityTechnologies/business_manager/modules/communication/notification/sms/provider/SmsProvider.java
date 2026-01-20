package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.provider;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsAccount;

public interface SmsProvider {
    SmsProviderResponse send(
            String to,
            String message,
            SmsAccount account
    );
}