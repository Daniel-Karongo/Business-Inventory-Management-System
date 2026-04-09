package com.IntegrityTechnologies.business_manager.security.device.service;

import com.IntegrityTechnologies.business_manager.security.biometric.repository.UserBiometricRepository;
import com.IntegrityTechnologies.business_manager.security.device.model.TrustedDevice;
import com.IntegrityTechnologies.business_manager.security.device.repository.TrustedDeviceRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class DeviceSecurityService {

    private final TrustedDeviceRepository repository;
    private final UserBiometricRepository biometricRepository;

    public void validate(UUID tenantId, UUID branchId, String fingerprint) {

        var deviceOpt = repository
                .findByTenantIdAndBranchIdAndFingerprint(tenantId, branchId, fingerprint);

        if (deviceOpt.isEmpty()) {

            boolean firstDevice =
                    !repository.existsByTenantIdAndBranchId(tenantId, branchId);

            repository.save(
                    TrustedDevice.builder()
                            .tenantId(tenantId)
                            .branchId(branchId)
                            .fingerprint(fingerprint)
                            .deviceName("Device-" + fingerprint.substring(0, 6))
                            .approved(firstDevice)
                            .build()
            );

            if (!firstDevice) {
                throw new SecurityException("Device pending approval");
            }

            return;
        }

        var device = deviceOpt.get();

        if (!Boolean.TRUE.equals(device.getApproved())) {
            throw new SecurityException("Device not approved for this branch");
        }

        device.setLastSeenAt(LocalDateTime.now());
        repository.save(device);
    }

    public boolean hasAnyDeviceForTenantBranch(UUID tenantId, UUID branchId) {
        return repository.existsByTenantIdAndBranchId(tenantId, branchId);
    }

    @Value("${security.device.max-per-user:3}")
    private int maxDevicesPerUser;

    public void enforceDeviceLimit(UUID tenantId, UUID userId) {

        long count = biometricRepository.countByTenantIdAndUserIdAndDeletedFalse(tenantId, userId);

        if (count >= maxDevicesPerUser) {
            throw new SecurityException("Maximum registered devices reached");
        }
    }

    public TrustedDevice getByFingerprint(UUID tenantId, UUID branchId, String fingerprint) {

        return repository
                .findByTenantIdAndBranchIdAndFingerprint(tenantId, branchId, fingerprint)
                .orElseThrow();
    }
}