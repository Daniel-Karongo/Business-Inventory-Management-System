package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.service;

import com.IntegrityTechnologies.business_manager.config.util.SecretMaskingUtil;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.base.BranchNotificationChannelPolicyService;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.dto.BranchEmailSettingsDTO;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.dto.BranchEmailSettingsResponseDTO;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model.BranchEmailSettings;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.repository.BranchEmailSettingsRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.persistence.EntityNotFoundException;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class BranchEmailSettingsService {

    private final BranchEmailSettingsRepository repo;
    private final BranchTenantGuard branchTenantGuard;
    private final BranchNotificationChannelPolicyService policyService;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    public BranchEmailSettingsResponseDTO get(UUID branchId) {

        branchTenantGuard.validate(branchId);

        UUID tenantId = tenantId();

        BranchEmailSettings settings =
                repo
                        .findByTenantIdAndBranchIdAndDeletedFalse(
                                tenantId,
                                branchId
                        )
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "Email settings not found"
                                )
                        );

        boolean effectiveEnabled =
                policyService.resolveEmailEnabled(
                        tenantId,
                        branchId,
                        settings
                );

        return BranchEmailSettingsResponseDTO
                .builder()
                .enabled(effectiveEnabled)
                .host(settings.getHost())
                .port(settings.getPort())
                .username(settings.getUsername())
                .password(
                        SecretMaskingUtil.mask(
                                settings.getPassword()
                        )
                )
                .fromAddress(settings.getFromAddress())
                .authEnabled(settings.getAuthEnabled())
                .tlsEnabled(settings.getTlsEnabled())
                .active(settings.getActive())
                .build();
    }

    @Transactional
    public BranchEmailSettings update(
            UUID branchId,
            BranchEmailSettingsDTO dto
    ) {

        branchTenantGuard.validate(branchId);

        UUID tenantId = tenantId();

        BranchEmailSettings settings =
                repo.findByTenantIdAndBranchIdAndDeletedFalse(
                                tenantId,
                                branchId
                        )
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "Email settings not found"
                                )
                        );

        boolean globalEnabled =
                policyService.isEmailGloballyEnabled(
                        tenantId,
                        branchId
                );

        settings.setEnabled(
                globalEnabled
                        && Boolean.TRUE.equals(dto.getEnabled())
        );

        settings.setHost(dto.getHost());
        settings.setPort(dto.getPort());
        settings.setUsername(dto.getUsername());

        if (dto.getPassword() != null
                && !dto.getPassword().isBlank()) {
            settings.setPassword(dto.getPassword());
        }

        settings.setFromAddress(dto.getFromAddress());
        settings.setAuthEnabled(dto.getAuthEnabled());
        settings.setTlsEnabled(dto.getTlsEnabled());
        settings.setActive(dto.getActive());

        return repo.save(settings);
    }
}