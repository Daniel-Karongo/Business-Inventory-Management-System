package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto.BranchSmsSettingsDTO;
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

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    public BranchSmsSettings get(UUID branchId) {

        branchTenantGuard.validate(branchId);

        return repo
                .findByTenantIdAndBranchIdAndDeletedFalse(
                        tenantId(),
                        branchId
                )
                .orElseThrow(() ->
                        new EntityNotFoundException(
                                "SMS settings not found"
                        )
                );
    }

    @Transactional
    public BranchSmsSettings update(
            UUID branchId,
            BranchSmsSettingsDTO dto
    ) {

        branchTenantGuard.validate(branchId);

        BranchSmsSettings settings =
                repo.findByTenantIdAndBranchIdAndDeletedFalse(
                                tenantId(),
                                branchId
                        )
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "SMS settings not found"
                                )
                        );

        settings.setEnabled(dto.getEnabled());
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