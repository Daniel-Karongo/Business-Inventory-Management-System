package com.IntegrityTechnologies.business_manager.security.device.service;

import com.IntegrityTechnologies.business_manager.security.device.dto.DeviceStatsDTO;
import com.IntegrityTechnologies.business_manager.security.device.dto.TrustedDeviceDTO;
import com.IntegrityTechnologies.business_manager.security.device.model.DeviceApprovalStatus;
import com.IntegrityTechnologies.business_manager.security.device.model.TrustedDevice;
import com.IntegrityTechnologies.business_manager.security.device.repository.DeviceUsageRepository;
import com.IntegrityTechnologies.business_manager.security.device.repository.TrustedDeviceRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class TenantTrustedDeviceManagementService {

    private final TrustedDeviceRepository repository;
    private final DeviceUsageRepository usageRepository;
    private final DeviceApprovalAuditService approvalAuditService;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    public List<TrustedDeviceDTO> list(UUID branchId) {

        if (branchId == null) {
            return repository
                    .findByTenantIdAndBranchIdIsNull(tenantId())
                    .stream()
                    .map(this::toDto)
                    .toList();
        }

        return repository
                .findByTenantIdAndBranchId(
                        tenantId(),
                        branchId
                )
                .stream()
                .map(this::toDto)
                .toList();
    }

    public List<TrustedDeviceDTO> pendingDevices() {

        return repository
                .findByTenantIdAndStatus(
                        tenantId(),
                        DeviceApprovalStatus.PENDING
                )
                .stream()
                .map(this::toDto)
                .toList();
    }

    @Transactional
    public void approve(UUID id, String reason) {

        TrustedDevice d =
                repository
                        .findByIdAndTenantId(id, tenantId())
                        .orElseThrow();

        d.setStatus(DeviceApprovalStatus.APPROVED);

        repository.save(d);

        approvalAuditService.log(
                d.getId(),
                "APPROVED",
                reason
        );
    }

    @Transactional
    public void reject(UUID id, String reason) {

        TrustedDevice d =
                repository
                        .findByIdAndTenantId(id, tenantId())
                        .orElseThrow();

        d.setStatus(DeviceApprovalStatus.REJECTED);

        repository.save(d);

        approvalAuditService.log(
                d.getId(),
                "REJECTED",
                reason
        );
    }

    @Transactional
    public void rename(
            UUID id,
            String name
    ) {
        TrustedDevice d =
                repository
                        .findByIdAndTenantId(id, tenantId())
                        .orElseThrow();

        d.setDeviceName(name);

        repository.save(d);
    }

    private TrustedDeviceDTO toDto(TrustedDevice d) {

        var users =
                usageRepository
                        .findByTenantIdAndDeviceId(
                                tenantId(),
                                d.getId()
                        )
                        .stream()
                        .map(x -> x.getUserId())
                        .distinct()
                        .toList();

        return TrustedDeviceDTO.builder()
                .id(d.getId())
                .branchId(d.getBranchId())
                .deviceName(d.getDeviceName())
                .deviceId(d.getDeviceId())
                .status(d.getStatus().name())
                .firstSeenAt(d.getFirstSeenAt())
                .lastSeenAt(d.getLastSeenAt())
                .usedByUserIds(users)
                .build();
    }

    public DeviceStatsDTO stats() {

        return DeviceStatsDTO.builder()
                .approvedDevices(
                        repository.countByTenantIdAndStatus(
                                tenantId(),
                                DeviceApprovalStatus.APPROVED
                        )
                )
                .pendingDevices(
                        repository.countByTenantIdAndStatus(
                                tenantId(),
                                DeviceApprovalStatus.PENDING
                        )
                )
                .rejectedDevices(
                        repository.countByTenantIdAndStatus(
                                tenantId(),
                                DeviceApprovalStatus.REJECTED
                        )
                )
                .devicesInUse(
                        usageRepository.countDistinctUserIdByTenantId(
                                tenantId()
                        )
                )
                .build();
    }
}