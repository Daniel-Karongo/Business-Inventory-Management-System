package com.IntegrityTechnologies.business_manager.security.device.service;

import com.IntegrityTechnologies.business_manager.security.device.repository.TrustedDeviceRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class DeviceSecurityService {

    private final TrustedDeviceRepository repository;

    public void validate(UUID tenantId, UUID branchId, String fingerprint) {

        var device = repository
                .findByTenantIdAndBranchIdAndFingerprint(tenantId, branchId, fingerprint)
                .orElseThrow(() -> new SecurityException("Device not registered for this branch"));

        if (!Boolean.TRUE.equals(device.getApproved())) {
            throw new SecurityException("Device not approved");
        }

        device.setLastSeenAt(LocalDateTime.now());
        repository.save(device);
    }
}