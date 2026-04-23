package com.IntegrityTechnologies.business_manager.security.biometric.service;

import com.IntegrityTechnologies.business_manager.security.biometric.dto.UserBiometricDTO;
import com.IntegrityTechnologies.business_manager.security.biometric.model.UserBiometric;
import com.IntegrityTechnologies.business_manager.security.biometric.repository.UserBiometricRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import com.IntegrityTechnologies.business_manager.security.biometric.dto.BiometricStatsDTO;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class UserBiometricService {

    private final UserBiometricRepository repository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    /* ================= LIST ================= */

    public List<UserBiometricDTO> listForUser(UUID userId) {

        return repository
                .findByTenantIdAndUserIdAndDeletedFalse(tenantId(), userId)
                .stream()
                .map(b -> UserBiometricDTO.builder()
                        .id(b.getId())
                        .deviceName(b.getDeviceName())
                        .deviceId(b.getDeviceId())
                        .build()
                )
                .toList();
    }

    public List<UserBiometricDTO> listForAdminUser(UUID userId) {

        return repository
                .findByTenantIdAndUserIdAndDeletedFalse(
                        tenantId(),
                        userId
                )
                .stream()
                .map(b -> UserBiometricDTO.builder()
                        .id(b.getId())
                        .deviceName(b.getDeviceName())
                        .deviceId(b.getDeviceId())
                        .build()
                )
                .toList();
    }

    public BiometricStatsDTO stats() {

        var all =
                repository.findByTenantIdAndDeletedFalse(
                        tenantId()
                );

        Set<UUID> users =
                all.stream()
                        .map(UserBiometric::getUserId)
                        .collect(Collectors.toSet());

        Set<String> devices =
                all.stream()
                        .map(UserBiometric::getDeviceId)
                        .collect(Collectors.toSet());

        return BiometricStatsDTO.builder()
                .activeCredentials(all.size())
                .uniqueUsers(users.size())
                .uniqueDevices(devices.size())
                .build();
    }

    /* ================= RENAME ================= */

    public void rename(UUID biometricId, UUID userId, String name) {

        UserBiometric b = repository
                .findByIdAndTenantId(biometricId, tenantId())
                .orElseThrow();

        if (!b.getUserId().equals(userId)) {
            throw new AccessDeniedException("Not your device");
        }

        b.setDeviceName(name);
        repository.save(b);
    }

    /* ================= DELETE ================= */

    public void delete(UUID biometricId, UUID userId, boolean hard) {

        UserBiometric b = repository
                .findByIdAndTenantId(biometricId, tenantId())
                .orElseThrow();

        if (!b.getUserId().equals(userId)) {
            throw new AccessDeniedException("Not your device");
        }

        long count = repository.countByTenantIdAndUserIdAndDeletedFalse(tenantId(), userId);

        if (count <= 1) {
            throw new IllegalStateException("Cannot delete last credential");
        }

        if (hard) {
            repository.delete(b);
        } else {
            b.setDeleted(true);
            b.setDeletedAt(LocalDateTime.now());
            repository.save(b);
        }
    }

    /* ================= ADMIN DELETE ================= */

    public void adminDelete(UUID biometricId, boolean hard) {

        UserBiometric b =
                repository
                        .findByIdAndTenantId(
                                biometricId,
                                tenantId()
                        )
                        .orElseThrow();

        long count =
                repository.countByTenantIdAndUserIdAndDeletedFalse(
                        tenantId(),
                        b.getUserId()
                );

        if(count <= 1){
            throw new IllegalStateException(
                    "Cannot delete last credential"
            );
        }

        if(hard){
            repository.delete(b);
            return;
        }

        b.setDeleted(true);
        b.setDeletedAt(LocalDateTime.now());

        repository.save(b);
    }
}