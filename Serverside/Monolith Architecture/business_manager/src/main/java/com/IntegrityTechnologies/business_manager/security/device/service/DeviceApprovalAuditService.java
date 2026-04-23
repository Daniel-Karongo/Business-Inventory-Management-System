package com.IntegrityTechnologies.business_manager.security.device.service;

import com.IntegrityTechnologies.business_manager.security.device.dto.DeviceApprovalAuditDTO;
import com.IntegrityTechnologies.business_manager.security.device.model.DeviceApprovalAudit;
import com.IntegrityTechnologies.business_manager.security.device.repository.DeviceApprovalAuditRepository;
import com.IntegrityTechnologies.business_manager.security.device.repository.TrustedDeviceRepository;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class DeviceApprovalAuditService {

    private final DeviceApprovalAuditRepository repo;
    private final TrustedDeviceRepository deviceRepository;

    public void log(
            java.util.UUID deviceId,
            String action,
            String reason
    ){
        repo.save(
                DeviceApprovalAudit.builder()
                        .tenantId(TenantContext.getTenantId())
                        .deviceId(deviceId)
                        .actedByUserId(SecurityUtils.currentUserId())
                        .action(action)
                        .reason(reason)
                        .build()
        );
    }

    public List<DeviceApprovalAuditDTO> history(
            java.util.UUID deviceId
    ){
        deviceRepository
                .findByIdAndTenantId(
                        deviceId,
                        TenantContext.getTenantId()
                )
                .orElseThrow();

        return repo
                .findByTenantIdAndDeviceIdOrderByActedAtDesc(
                        TenantContext.getTenantId(),
                        deviceId
                )
                .stream()
                .map(a ->
                        DeviceApprovalAuditDTO.builder()
                                .actedByUserId(a.getActedByUserId())
                                .action(a.getAction())
                                .reason(a.getReason())
                                .actedAt(a.getActedAt())
                                .build()
                )
                .toList();
    }
}