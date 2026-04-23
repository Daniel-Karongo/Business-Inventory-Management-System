package com.IntegrityTechnologies.business_manager.security.device.repository;

import com.IntegrityTechnologies.business_manager.security.device.model.DeviceApprovalAudit;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface DeviceApprovalAuditRepository
        extends JpaRepository<DeviceApprovalAudit,UUID> {
    List<DeviceApprovalAudit> findByTenantIdAndDeviceIdOrderByActedAtDesc(
            UUID tenantId,
            UUID deviceId
    );
}