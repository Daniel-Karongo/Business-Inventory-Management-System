package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsMessage;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.repository.SmsMessageRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Map;

@Service
@RequiredArgsConstructor
public class SmsDeliveryReportService {

    private final SmsMessageRepository repo;

    @Transactional
    public void handleDeliveryReport(Map<String, String> payload) {

        String messageId = payload.get("id");
        String status = payload.get("status");

        if (messageId == null) return;

        SmsMessage msg = repo
                .findAll()
                .stream()
                .filter(m -> messageId.equals(m.getProviderMessageId()))
                .findFirst()
                .orElse(null);

        if (msg == null) return;

        if ("Success".equalsIgnoreCase(status)) {
            msg.setStatus("DELIVERED");
        } else {
            msg.setStatus("FAILED");
            msg.setError(payload.toString());
        }

        repo.save(msg);
    }
}