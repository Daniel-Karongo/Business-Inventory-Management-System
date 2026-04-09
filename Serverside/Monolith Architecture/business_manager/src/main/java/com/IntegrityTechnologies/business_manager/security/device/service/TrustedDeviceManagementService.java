package com.IntegrityTechnologies.business_manager.security.device.service;

import com.IntegrityTechnologies.business_manager.security.device.dto.TrustedDeviceDTO;
import com.IntegrityTechnologies.business_manager.security.device.model.TrustedDevice;
import com.IntegrityTechnologies.business_manager.security.device.repository.DeviceUsageRepository;
import com.IntegrityTechnologies.business_manager.security.device.repository.TrustedDeviceRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class TrustedDeviceManagementService {

    private final TrustedDeviceRepository repository;
    private final DeviceUsageRepository deviceUsageRepository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    /* ================= LIST ================= */

    public List<TrustedDeviceDTO> list(UUID branchId) {

        UUID tenantId = tenantId();

        if (branchId == null) {
            // PLATFORM → branchId null
            return repository.findByTenantIdAndBranchId(tenantId, null)
                    .stream()
                    .map(this::toDto)
                    .toList();
        }

        return repository.findByTenantIdAndBranchId(tenantId, branchId)
                .stream()
                .map(this::toDto)
                .toList();
    }

    /* ================= APPROVE ================= */

    public void approve(UUID deviceId) {

        TrustedDevice d = repository
                .findByIdAndTenantId(deviceId, tenantId())
                .orElseThrow();

        d.setApproved(true);
        repository.save(d);
    }

    /* ================= REJECT ================= */

    public void reject(UUID deviceId) {

        TrustedDevice d = repository
                .findByIdAndTenantId(deviceId, tenantId())
                .orElseThrow();

        d.setApproved(false);
        repository.save(d);
    }

    public void rename(UUID deviceId, String name) {

        TrustedDevice d = repository
                .findByIdAndTenantId(deviceId, tenantId())
                .orElseThrow();

        d.setDeviceName(name);
        repository.save(d);
    }

    private TrustedDeviceDTO toDto(TrustedDevice d) {

        List<UUID> userIds = deviceUsageRepository
                .findByTenantIdAndDeviceId(tenantId(), d.getId())
                .stream()
                .map(u -> u.getUserId())
                .distinct()
                .toList();

        return TrustedDeviceDTO.builder()
                .id(d.getId())
                .branchId(d.getBranchId())
                .deviceName(d.getDeviceName())
                .deviceId(d.getDeviceId())
                .approved(d.getApproved())
                .firstSeenAt(d.getFirstSeenAt())
                .lastSeenAt(d.getLastSeenAt())
                .usedByUserIds(userIds)
                .build();
    }
}