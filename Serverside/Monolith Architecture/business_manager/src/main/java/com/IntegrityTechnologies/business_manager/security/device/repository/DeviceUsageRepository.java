package com.IntegrityTechnologies.business_manager.security.device.repository;

import com.IntegrityTechnologies.business_manager.security.device.model.DeviceUsage;
import jakarta.persistence.LockModeType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;

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

    @Query("""
        select count(distinct du.deviceId)
        from DeviceUsage du
        join TrustedDevice td on td.id = du.deviceId
        where du.tenantId = :tenantId
        and du.userId = :userId
        and (
         (:branchId is null and td.branchId is null)
         or td.branchId = :branchId
        )
    """)
    long countDistinctDevicesForUserInBranch(
            UUID tenantId,
            UUID userId,
            UUID branchId
    );

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("""
        select du
        from DeviceUsage du
        where du.tenantId=:tenantId
        and du.userId=:userId
    """)
    List<DeviceUsage> lockUserDevices(
            UUID tenantId,
            UUID userId
    );
}