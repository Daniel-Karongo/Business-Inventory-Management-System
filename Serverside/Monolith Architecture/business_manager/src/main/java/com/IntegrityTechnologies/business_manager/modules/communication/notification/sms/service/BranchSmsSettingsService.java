package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service;

import com.IntegrityTechnologies.business_manager.config.util.SecretMaskingUtil;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.base.BranchNotificationChannelPolicyService;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto.BranchSmsSettingsDTO;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto.BranchSmsSettingsResponseDTO;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.BranchSmsSettings;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.repository.BranchSmsSettingsRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.persistence.EntityNotFoundException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class BranchSmsSettingsService {

    private final BranchSmsSettingsRepository repo;
    private final BranchTenantGuard branchTenantGuard;
    private final BranchNotificationChannelPolicyService policyService;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    public BranchSmsSettingsResponseDTO get(UUID branchId) {

        branchTenantGuard.validate(branchId);

        UUID tenantId = tenantId();

        BranchSmsSettings settings =
                repo
                        .findByTenantIdAndBranchIdAndDeletedFalse(
                                tenantId,
                                branchId
                        )
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "SMS settings not found"
                                )
                        );

        boolean effectiveEnabled =
                policyService.resolveSmsEnabled(
                        tenantId,
                        branchId,
                        settings
                );

        return BranchSmsSettingsResponseDTO
                .builder()
                .enabled(effectiveEnabled)
                .provider(settings.getProvider())
                .username(settings.getUsername())
                .apiKey(
                        SecretMaskingUtil.mask(
                                settings.getApiKey()
                        )
                )
                .senderId(settings.getSenderId())
                .defaultCountryCode(
                        settings.getDefaultCountryCode()
                )
                .sandbox(settings.getSandbox())
                .active(settings.getActive())
                .build();
    }

    @Transactional
    public BranchSmsSettings update(
            UUID branchId,
            BranchSmsSettingsDTO dto
    ) {

        branchTenantGuard.validate(branchId);

        UUID tenantId = tenantId();

        BranchSmsSettings settings =
                repo.findByTenantIdAndBranchIdAndDeletedFalse(
                                tenantId,
                                branchId
                        )
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "SMS settings not found"
                                )
                        );

        boolean globalEnabled =
                policyService.isSmsGloballyEnabled(
                        tenantId,
                        branchId
                );

        settings.setEnabled(
                globalEnabled
                        && Boolean.TRUE.equals(dto.getEnabled())
        );

        settings.setProvider(dto.getProvider());
        settings.setUsername(dto.getUsername());

        if (dto.getApiKey() != null
                && !dto.getApiKey().isBlank()) {
            settings.setApiKey(dto.getApiKey());
        }

        settings.setSenderId(dto.getSenderId());

        settings.setDefaultCountryCode(
                dto.getDefaultCountryCode()
        );

        settings.setSandbox(dto.getSandbox());
        settings.setActive(dto.getActive());

        return repo.save(settings);
    }
}