package com.IntegrityTechnologies.business_manager.modules.platform.settings.service;

import com.IntegrityTechnologies.business_manager.modules.platform.settings.repository.TenantSettingsRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.TenantSettings;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class TenantSettingsService {

    private final TenantSettingsRepository repository;

    public TenantSettings getSettings() {

        UUID tenantId = TenantContext.getTenantId();

        return repository.findById(tenantId)
                .orElseGet(() ->
                        repository.save(
                                TenantSettings.builder()
                                        .tenantId(tenantId)
                                        .currency("KES")
                                        .timezone("Africa/Nairobi")
                                        .locale("en-KE")
                                        .build()
                        )
                );
    }

    public void updateLogo(String logoPath) {

        TenantSettings settings = getSettings();

        settings.setLogoUrl(logoPath);

        repository.save(settings);

    }

}