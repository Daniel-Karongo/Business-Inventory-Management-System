package com.IntegrityTechnologies.business_manager.modules.communication.notification.base;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model.BranchEmailSettings;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.BranchSmsSettings;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class BranchNotificationChannelPolicyService {

    private final BranchNotificationSettingsRepository notificationRepo;

    public boolean isSmsGloballyEnabled(
            UUID tenantId,
            UUID branchId
    ) {
        return notificationRepo
                .findByTenantIdAndBranchIdAndDeletedFalse(
                        tenantId,
                        branchId
                )
                .map(BranchNotificationSettings::getSmsEnabled)
                .orElse(false);
    }

    public boolean isEmailGloballyEnabled(
            UUID tenantId,
            UUID branchId
    ) {
        return notificationRepo
                .findByTenantIdAndBranchIdAndDeletedFalse(
                        tenantId,
                        branchId
                )
                .map(BranchNotificationSettings::getEmailEnabled)
                .orElse(false);
    }

    public boolean resolveSmsEnabled(
            UUID tenantId,
            UUID branchId,
            BranchSmsSettings settings
    ) {
        return isSmsGloballyEnabled(
                tenantId,
                branchId
        ) && Boolean.TRUE.equals(
                settings.getEnabled()
        );
    }

    public boolean resolveEmailEnabled(
            UUID tenantId,
            UUID branchId,
            BranchEmailSettings settings
    ) {
        return isEmailGloballyEnabled(
                tenantId,
                branchId
        ) && Boolean.TRUE.equals(
                settings.getEnabled()
        );
    }
}