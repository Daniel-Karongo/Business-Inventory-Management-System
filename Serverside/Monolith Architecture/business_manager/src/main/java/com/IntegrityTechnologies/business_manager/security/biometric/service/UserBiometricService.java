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
                        .fingerprint(b.getFingerprint())
                        .build()
                )
                .toList();
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

        UserBiometric b = repository
                .findByIdAndTenantId(biometricId, tenantId())
                .orElseThrow();

        if (hard) {
            repository.delete(b);
        } else {
            b.setDeleted(true);
            b.setDeletedAt(LocalDateTime.now());
            repository.save(b);
        }
    }
}