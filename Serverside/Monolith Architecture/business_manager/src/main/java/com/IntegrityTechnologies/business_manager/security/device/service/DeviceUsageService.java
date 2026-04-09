package com.IntegrityTechnologies.business_manager.security.device.service;

import com.IntegrityTechnologies.business_manager.security.device.model.DeviceUsage;
import com.IntegrityTechnologies.business_manager.security.device.repository.DeviceUsageRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class DeviceUsageService {

    private final DeviceUsageRepository repository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    public void record(UUID deviceId, UUID userId) {

        UUID tenantId = tenantId();

        var existing = repository
                .findByTenantIdAndDeviceIdAndUserId(tenantId, deviceId, userId);

        if (existing.isPresent()) {
            DeviceUsage usage = existing.get();
            usage.setLastUsedAt(LocalDateTime.now());
            repository.save(usage);
            return;
        }

        repository.save(
                DeviceUsage.builder()
                        .tenantId(tenantId)
                        .deviceId(deviceId)
                        .userId(userId)
                        .lastUsedAt(LocalDateTime.now())
                        .build()
        );
    }
}