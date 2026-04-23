package com.IntegrityTechnologies.business_manager.security.device.service;

import com.IntegrityTechnologies.business_manager.security.device.dto.TrustedDeviceDTO;
import com.IntegrityTechnologies.business_manager.security.device.model.DeviceApprovalStatus;
import com.IntegrityTechnologies.business_manager.security.device.repository.TrustedDeviceRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class PlatformTrustedDeviceManagementService {

    private final TrustedDeviceRepository repository;
    private final DeviceApprovalAuditService auditService;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    public List<TrustedDeviceDTO> listPlatformDevices() {

        return repository
                .findByTenantIdAndBranchIdIsNull(
                        tenantId()
                )
                .stream()
                .map(d ->
                        TrustedDeviceDTO.builder()
                                .id(d.getId())
                                .deviceName(d.getDeviceName())
                                .deviceId(d.getDeviceId())
                                .status(d.getStatus().name())
                                .firstSeenAt(d.getFirstSeenAt())
                                .lastSeenAt(d.getLastSeenAt())
                                .build()
                )
                .toList();
    }

    @Transactional
    public void approve(UUID id, String reason){

        var d =
                repository
                        .findByIdAndTenantId(
                                id,
                                tenantId()
                        )
                        .orElseThrow();

        d.setStatus(DeviceApprovalStatus.APPROVED);

        repository.save(d);

        auditService.log(
                d.getId(),
                "APPROVED",
                reason
        );
    }

    @Transactional
    public void reject(UUID id, String reason){

        var d =
                repository
                        .findByIdAndTenantId(
                                id,
                                tenantId()
                        )
                        .orElseThrow();

        d.setStatus(DeviceApprovalStatus.REJECTED);

        repository.save(d);

        auditService.log(
                d.getId(),
                "REJECTED",
                reason
        );
    }
}