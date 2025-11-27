package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto;

import lombok.Data;

@Data
public class SmsRequest {
    private String toPhone;
    private String message;
    private String from; // optional override
    private String createdBy;
}