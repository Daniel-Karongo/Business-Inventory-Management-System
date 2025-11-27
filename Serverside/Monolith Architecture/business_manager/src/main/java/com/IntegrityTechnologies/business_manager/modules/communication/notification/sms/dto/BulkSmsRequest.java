package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto;

import lombok.Data;

import java.util.List;

@Data
public class BulkSmsRequest {
    private List<SmsRequest> messages;
}