package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto.SmsRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsMessage;

import java.util.List;
import java.util.UUID;

public interface SmsService {
    SmsMessage sendSms(SmsRequest req);
    List<SmsMessage> sendBulk(List<SmsRequest> requests);
    SmsMessage getMessage(UUID id);
}