package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.config.SmsProperties;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.client.SmsClient;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto.SmsRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsMessage;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.repository.SmsMessageRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class SmsServiceImpl implements SmsService {

    private final SmsMessageRepository repo;
    private final SmsClient client;
    private final SmsProperties props;

    @Override
    @Transactional
    public SmsMessage sendSms(SmsRequest req) {
        SmsMessage m = SmsMessage.builder()
                .id(UUID.randomUUID())
                .toPhone(normalizePhone(req.getToPhone()))
                .fromName(req.getFrom() != null ? req.getFrom() : props.getSenderName())
                .message(req.getMessage())
                .status("PENDING")
                .createdAt(LocalDateTime.now())
                .createdBy(req.getCreatedBy())
                .build();
        repo.save(m);
        try {
            Map response = client.sendSms(m.getToPhone(), m.getMessage(), m.getFromName());
            // best-effort map reading
            if (response != null && (response.get("status") != null || response.get("ConversationID") != null || response.get("requestId") != null)) {
                m.setStatus("SENT");
                m.setProviderMessageId(response.getOrDefault("requestId", response.get("ConversationID")).toString());
                m.setSentAt(LocalDateTime.now());
            } else {
                m.setStatus("SENT"); // many providers return HTTP 200 but detail in body — mark SENT and let delivery webhooks handle actual delivery
                m.setSentAt(LocalDateTime.now());
            }
        } catch (Exception ex) {
            m.setStatus("FAILED");
            m.setError(ex.getMessage());
        }
        return repo.save(m);
    }

    @Override
    @Transactional
    public List<SmsMessage> sendBulk(List<SmsRequest> requests) {
        List<SmsMessage> out = new ArrayList<>();
        for (SmsRequest r : requests) {
            out.add(sendSms(r));
        }
        return out;
    }

    @Override
    public SmsMessage getMessage(UUID id) {
        return repo.findById(id).orElse(null);
    }

    private String normalizePhone(String phone) {
        if (phone == null) return null;
        String p = phone.trim();
        if (p.startsWith("0") && props.getDefaultCountryCode() != null) {
            // convert 0712345678 -> +254712345678 if default is +254
            return props.getDefaultCountryCode() + p.substring(1);
        }
        if (!p.startsWith("+") && props.getDefaultCountryCode() != null) {
            return props.getDefaultCountryCode() + p;
        }
        return p;
    }
}