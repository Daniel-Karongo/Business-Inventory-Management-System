package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto.SmsRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.BranchSmsSettings;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsMessage;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.repository.BranchSmsSettingsRepository;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.repository.SmsMessageRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.base.BranchNotificationSettings;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.base.BranchNotificationSettingsRepository;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.provider.BranchSmsProviderAdapter;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.provider.SmsProviderResponse;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;

import jakarta.persistence.EntityNotFoundException;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class SmsServiceImpl implements SmsService {

    private final SmsMessageRepository messageRepo;
    private final BranchSmsSettingsRepository settingsRepo;
    private final BranchTenantGuard branchTenantGuard;
    private final BranchSmsProviderAdapter providerAdapter;
    private final BranchNotificationSettingsRepository notificationSettingsRepository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Override
    @Transactional
    public SmsMessage sendSms(
            UUID branchId,
            SmsRequest req
    ) {

        branchTenantGuard.validate(branchId);

        BranchSmsSettings settings =
                settingsRepo
                        .findByTenantIdAndBranchIdAndActiveTrueAndDeletedFalse(
                                tenantId(),
                                branchId
                        )
                        .orElseThrow(() ->
                                new IllegalStateException(
                                        "SMS not configured for branch"
                                )
                        );

        if (!Boolean.TRUE.equals(settings.getEnabled())) {

            throw new IllegalStateException(
                    "SMS disabled for branch"
            );
        }

        SmsMessage message =
                SmsMessage.builder()
                        .tenantId(tenantId())
                        .branchId(branchId)
                        .toPhone(
                                normalize(
                                        req.getToPhone(),
                                        settings.getDefaultCountryCode()
                                )
                        )
                        .fromName(settings.getSenderId())
                        .message(req.getMessage())
                        .status("QUEUED")
                        .retryCount(0)
                        .createdBy(req.getCreatedBy())
                        .build();

        message =
                messageRepo.save(message);

        messageRepo.flush();

        dispatchSms(
                tenantId(),
                branchId,
                message.getId()
        );

        return message;
    }

    @Override
    @Transactional
    public List<SmsMessage> sendBulk(
            UUID branchId,
            List<SmsRequest> requests
    ) {

        branchTenantGuard.validate(branchId);

        List<SmsMessage> out = new ArrayList<>();

        for (SmsRequest r : requests) {
            out.add(sendSms(branchId, r));
        }

        return out;
    }

    @Override
    @Transactional
    public void dispatchSms(
            UUID tenantId,
            UUID branchId,
            UUID messageId
    ) {

        TenantContext.setTenantId(tenantId);

        try {

            SmsMessage message =
                    messageRepo
                            .findByTenantIdAndBranchIdAndId(
                                    tenantId,
                                    branchId,
                                    messageId
                            )
                            .orElseThrow(() ->
                                    new EntityNotFoundException(
                                            "SMS message not found"
                                    )
                            );

            if ("SENT".equalsIgnoreCase(message.getStatus())
                    || "DELIVERED".equalsIgnoreCase(message.getStatus())) {

                return;
            }

            BranchSmsSettings settings =
                    settingsRepo
                            .findByTenantIdAndBranchIdAndActiveTrueAndDeletedFalse(
                                    tenantId,
                                    branchId
                            )
                            .orElseThrow(() ->
                                    new IllegalStateException(
                                            "SMS settings missing"
                                    )
                            );

            SmsProviderResponse response =
                    providerAdapter.send(
                            settings,
                            message.getToPhone(),
                            message.getMessage()
                    );

            if (response.isSuccess()) {

                message.setStatus("SENT");
                message.setSentAt(LocalDateTime.now());
                message.setProviderMessageId(
                        response.getProviderMessageId()
                );
                message.setError(null);

            } else {

                message.setStatus("FAILED");
                message.setError(response.getError());

                scheduleRetry(
                        tenantId,
                        branchId,
                        message
                );
            }

            messageRepo.save(message);

        } finally {

            TenantContext.clear();
        }
    }

    private void scheduleRetry(
            UUID tenantId,
            UUID branchId,
            SmsMessage message
    ) {

        BranchNotificationSettings settings =
                notificationSettingsRepository
                        .findByTenantIdAndBranchIdAndDeletedFalse(
                                tenantId,
                                branchId
                        )
                        .orElse(null);

        if (settings == null) {
            return;
        }

        if (!Boolean.TRUE.equals(settings.getAllowRetries())) {
            return;
        }

        int maxRetries =
                settings.getMaxRetryCount();

        if (message.getRetryCount() >= maxRetries) {
            return;
        }

        int nextRetry =
                Math.max(
                        1,
                        message.getRetryCount()
                );

        message.setNextRetryAt(
                LocalDateTime.now()
                        .plusMinutes(nextRetry * 5L)
        );
    }

    @Override
    public SmsMessage getMessage(
            UUID branchId,
            UUID id
    ) {

        branchTenantGuard.validate(branchId);

        return messageRepo
                .findByTenantIdAndBranchIdAndId(
                        tenantId(),
                        branchId,
                        id
                )
                .orElseThrow(() ->
                        new EntityNotFoundException(
                                "SMS message not found"
                        )
                );
    }

    private String normalize(
            String phone,
            String countryCode
    ) {

        if (phone == null) {
            return null;
        }

        String p = phone.trim();

        if (p.startsWith("0")) {
            return countryCode + p.substring(1);
        }

        if (!p.startsWith("+")) {
            return countryCode + p;
        }

        return p;
    }
}