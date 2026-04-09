package com.IntegrityTechnologies.business_manager.security.device.repository;

import com.IntegrityTechnologies.business_manager.security.device.model.DeviceUsage;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface DeviceUsageRepository extends JpaRepository<DeviceUsage, UUID> {

    Optional<DeviceUsage> findByTenantIdAndDeviceIdAndUserId(
            UUID tenantId,
            UUID deviceId,
            UUID userId
    );

    List<DeviceUsage> findByTenantIdAndDeviceId(UUID tenantId, UUID deviceId);
}