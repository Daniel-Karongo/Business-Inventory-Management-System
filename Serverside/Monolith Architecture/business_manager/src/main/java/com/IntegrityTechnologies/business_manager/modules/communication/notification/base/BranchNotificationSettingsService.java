package com.IntegrityTechnologies.business_manager.modules.communication.notification.base;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.repository.BranchEmailSettingsRepository;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.repository.BranchSmsSettingsRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.persistence.EntityNotFoundException;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class BranchNotificationSettingsService {

    private final BranchNotificationSettingsRepository repo;
    private final BranchTenantGuard branchTenantGuard;
    private final BranchEmailSettingsRepository emailRepo;
    private final BranchSmsSettingsRepository smsRepo;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    public BranchNotificationSettings get(
            UUID branchId
    ) {

        branchTenantGuard.validate(branchId);

        return repo
                .findByTenantIdAndBranchIdAndDeletedFalse(
                        tenantId(),
                        branchId
                )
                .orElseThrow(() ->
                        new EntityNotFoundException(
                                "Notification settings not found"
                        )
                );
    }

    @Transactional
    public BranchNotificationSettings update(
            UUID branchId,
            BranchNotificationSettingsDTO dto
    ) {

        branchTenantGuard.validate(branchId);

        BranchNotificationSettings settings =
                repo.findByTenantIdAndBranchIdAndDeletedFalse(
                                tenantId(),
                                branchId
                        )
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "Notification settings not found"
                                )
                        );

        if (dto.getSmsEnabled() != null) {
            settings.setSmsEnabled(
                    dto.getSmsEnabled()
            );
        }

        if (dto.getEmailEnabled() != null) {
            settings.setEmailEnabled(
                    dto.getEmailEnabled()
            );
        }

        if (dto.getAllowRetries() != null) {
            settings.setAllowRetries(
                    dto.getAllowRetries()
            );
        }

        if (dto.getMaxRetryCount() != null) {

            if (dto.getMaxRetryCount() < 0) {
                throw new IllegalArgumentException(
                        "maxRetryCount cannot be negative"
                );
            }

            settings.setMaxRetryCount(
                    dto.getMaxRetryCount()
            );
        }

        syncChannelStates(
                tenantId(),
                branchId,
                settings
        );

        return repo.save(settings);
    }

    private void syncChannelStates(
            UUID tenantId,
            UUID branchId,
            BranchNotificationSettings settings
    ) {

        if (!Boolean.TRUE.equals(settings.getEmailEnabled())) {

            emailRepo
                    .findByTenantIdAndBranchIdAndDeletedFalse(
                            tenantId,
                            branchId
                    )
                    .ifPresent(email -> {
                        email.setEnabled(false);
                        emailRepo.save(email);
                    });
        }

        if (!Boolean.TRUE.equals(settings.getSmsEnabled())) {

            smsRepo
                    .findByTenantIdAndBranchIdAndDeletedFalse(
                            tenantId,
                            branchId
                    )
                    .ifPresent(sms -> {
                        sms.setEnabled(false);
                        smsRepo.save(sms);
                    });
        }
    }
}