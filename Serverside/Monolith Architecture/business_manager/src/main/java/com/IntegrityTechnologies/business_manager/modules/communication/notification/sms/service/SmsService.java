package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto.SmsRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsMessage;

import java.util.List;
import java.util.UUID;

public interface SmsService {

    SmsMessage sendSms(
            UUID branchId,
            SmsRequest req
    );

    List<SmsMessage> sendBulk(
            UUID branchId,
            List<SmsRequest> requests
    );

    SmsMessage getMessage(
            UUID branchId,
            UUID id
    );

    void dispatchSms(
            UUID tenantId,
            UUID branchId,
            UUID messageId
    );
}