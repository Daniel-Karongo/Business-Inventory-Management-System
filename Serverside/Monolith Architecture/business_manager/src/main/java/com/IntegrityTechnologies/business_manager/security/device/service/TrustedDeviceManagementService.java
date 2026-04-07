package com.IntegrityTechnologies.business_manager.security.device.service;

import com.IntegrityTechnologies.business_manager.security.device.dto.TrustedDeviceDTO;
import com.IntegrityTechnologies.business_manager.security.device.model.TrustedDevice;
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

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    /* ================= LIST ================= */

    public List<TrustedDeviceDTO> list(UUID branchId) {

        return repository.findByTenantIdAndBranchId(tenantId(), branchId)
                .stream()
                .map(d -> TrustedDeviceDTO.builder()
                        .id(d.getId())
                        .branchId(d.getBranchId())
                        .deviceName(d.getDeviceName())
                        .fingerprint(d.getFingerprint())
                        .approved(d.getApproved())
                        .firstSeenAt(d.getFirstSeenAt())
                        .lastSeenAt(d.getLastSeenAt())
                        .build()
                )
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
}