package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.config.SmsProperties;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto.SmsRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsAccount;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsMessage;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.repository.SmsAccountRepository;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.repository.SmsMessageRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class SmsServiceImpl implements SmsService {

    private final SmsMessageRepository messageRepo;
    private final SmsAccountRepository accountRepo;
    private final SmsProperties props;

    @Override
    @Transactional
    public SmsMessage sendSms(SmsRequest req) {

        SmsAccount account = accountRepo.findByActiveTrue()
                .orElseThrow(() -> new IllegalStateException("SMS not configured"));

        SmsMessage message = SmsMessage.builder()
                .toPhone(normalize(req.getToPhone()))
                .fromName(account.getSenderId())
                .message(req.getMessage())
                .status("QUEUED")
                .retryCount(0)
                .createdAt(LocalDateTime.now())
                .createdBy(req.getCreatedBy())
                .build();

        return messageRepo.save(message);
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
        return messageRepo.findById(id).orElse(null);
    }

    private String normalize(String phone) {
        if (phone == null) return null;
        String p = phone.trim();
        if (p.startsWith("0")) {
            return props.getDefaultCountryCode() + p.substring(1);
        }
        if (!p.startsWith("+")) {
            return props.getDefaultCountryCode() + p;
        }
        return p;
    }
}