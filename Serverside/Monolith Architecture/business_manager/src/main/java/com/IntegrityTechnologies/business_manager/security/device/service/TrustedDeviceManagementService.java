package com.IntegrityTechnologies.business_manager.security.device.service;

import com.IntegrityTechnologies.business_manager.security.audit.repository.LoginAuditRepository;
import com.IntegrityTechnologies.business_manager.security.device.dto.TrustedDeviceDTO;
import com.IntegrityTechnologies.business_manager.security.device.model.DeviceApprovalStatus;
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
    private final LoginAuditRepository auditRepository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    public List<TrustedDeviceDTO> list(UUID branchId) {

        UUID tenantId = tenantId();

        if (branchId == null) {
            return repository
                    .findByTenantIdAndBranchId(tenantId,null)
                    .stream()
                    .map(this::toDto)
                    .toList();
        }

        return repository
                .findByTenantIdAndBranchId(
                        tenantId,
                        branchId
                )
                .stream()
                .map(this::toDto)
                .toList();
    }

    public void approve(UUID deviceId) {

        TrustedDevice d =
                repository
                        .findByIdAndTenantId(
                                deviceId,
                                tenantId()
                        )
                        .orElseThrow();

        d.setStatus(DeviceApprovalStatus.APPROVED);

        repository.save(d);
    }

    public void reject(UUID deviceId) {

        TrustedDevice d =
                repository
                        .findByIdAndTenantId(
                                deviceId,
                                tenantId()
                        )
                        .orElseThrow();

        d.setStatus(DeviceApprovalStatus.REJECTED);

        repository.save(d);
    }

    public void rename(
            UUID deviceId,
            String name
    ) {

        TrustedDevice d =
                repository
                        .findByIdAndTenantId(
                                deviceId,
                                tenantId()
                        )
                        .orElseThrow();

        d.setDeviceName(name);

        repository.save(d);
    }

    private TrustedDeviceDTO toDto(TrustedDevice d) {

        UUID tenantId = tenantId();

        List<UUID> usedByUsers =
                deviceUsageRepository
                        .findByTenantIdAndDeviceId(
                                tenantId,
                                d.getId()
                        )
                        .stream()
                        .map(x -> x.getUserId())
                        .distinct()
                        .toList();

        var audits =
                auditRepository.findByTenantIdAndFingerprint(
                        tenantId,
                        d.getDeviceId()
                );

        List<UUID> attemptedByUsers =
                audits.stream()
                        .map(a -> a.getUserId())
                        .filter(x -> x != null)
                        .distinct()
                        .toList();

        int pendingAttempts =
                (int) auditRepository
                        .countByTenantIdAndFingerprintAndReason(
                                tenantId,
                                d.getDeviceId(),
                                "DEVICE_PENDING_APPROVAL"
                        );

        String lastIp =
                audits.stream()
                        .map(a -> a.getIp())
                        .filter(x -> x != null)
                        .reduce((a,b)->b)
                        .orElse(null);

        return TrustedDeviceDTO.builder()
                .id(d.getId())
                .branchId(d.getBranchId())
                .deviceName(d.getDeviceName())
                .deviceId(d.getDeviceId())
                .status(d.getStatus().name())
                .firstSeenAt(d.getFirstSeenAt())
                .lastSeenAt(d.getLastSeenAt())
                .usedByUserIds(usedByUsers)
                .attemptedByUserIds(attemptedByUsers)
                .pendingAttemptCount(pendingAttempts)
                .ipAddress(lastIp)
                .build();
    }
}