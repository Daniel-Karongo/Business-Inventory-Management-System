package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.provider;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class SmsProviderResponse {
    private boolean success;
    private String providerMessageId;
    private String error;
}