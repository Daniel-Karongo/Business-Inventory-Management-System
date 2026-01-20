package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsAccount;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsMessage;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.provider.SmsProvider;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.provider.SmsProviderResponse;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.repository.SmsAccountRepository;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.repository.SmsMessageRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;

@Component
@RequiredArgsConstructor
public class SmsQueueProcessor {

    private static final int MAX_RETRIES = 2;

    private final SmsMessageRepository messageRepo;
    private final SmsAccountRepository accountRepo;
    private final SmsProvider provider;

    @Scheduled(fixedDelay = 10_000) // every 10 seconds
    @Transactional
    public void processQueue() {

        List<SmsMessage> pending = messageRepo
                .findByStatusInAndNextRetryAtBeforeOrNextRetryAtIsNull(
                        List.of("QUEUED", "RETRY"),
                        LocalDateTime.now()
                );

        if (pending.isEmpty()) {
            return;
        }

        for (SmsMessage msg : pending) {

            if ("SENT".equals(msg.getStatus())) {
                continue;
            }

            if (msg.getRetryCount() >= MAX_RETRIES) {
                msg.setStatus("FAILED");
                msg.setError("Max retries exceeded");
                continue;
            }

            SmsAccount account = accountRepo.findByActiveTrue()
                    .orElse(null);

            if (account == null) {
                msg.setStatus("FAILED");
                msg.setError("No active SMS account configured");
                continue;
            }

            SmsProviderResponse resp =
                    provider.send(msg.getToPhone(), msg.getMessage(), account);

            if (resp.isSuccess()) {
                msg.setStatus("SENT");
                msg.setProviderMessageId(resp.getProviderMessageId());
                msg.setSentAt(LocalDateTime.now());

                // 🔒 Prevent re-processing
                msg.setNextRetryAt(null);
                msg.setRetryCount(MAX_RETRIES); // hard stop
            } else {
                msg.setRetryCount(msg.getRetryCount() + 1);
                msg.setStatus("RETRY");
                msg.setNextRetryAt(LocalDateTime.now().plusMinutes(2));
                msg.setError(resp.getError());
            }
        }
    }
}