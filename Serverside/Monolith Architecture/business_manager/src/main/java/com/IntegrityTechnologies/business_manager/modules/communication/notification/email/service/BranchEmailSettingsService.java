package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.dto.BranchEmailSettingsDTO;
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

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    public BranchEmailSettings get(UUID branchId) {

        branchTenantGuard.validate(branchId);

        return repo
                .findByTenantIdAndBranchIdAndDeletedFalse(
                        tenantId(),
                        branchId
                )
                .orElseThrow(() ->
                        new EntityNotFoundException(
                                "Email settings not found"
                        )
                );
    }

    @Transactional
    public BranchEmailSettings update(
            UUID branchId,
            BranchEmailSettingsDTO dto
    ) {

        branchTenantGuard.validate(branchId);

        BranchEmailSettings settings =
                repo.findByTenantIdAndBranchIdAndDeletedFalse(
                                tenantId(),
                                branchId
                        )
                        .orElseThrow(() ->
                                new EntityNotFoundException(
                                        "Email settings not found"
                                )
                        );

        settings.setEnabled(dto.getEnabled());
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