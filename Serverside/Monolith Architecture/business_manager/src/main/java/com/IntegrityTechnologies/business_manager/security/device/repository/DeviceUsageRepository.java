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

    @Query("""
        select count(distinct du.userId)
        from DeviceUsage du
        where du.tenantId = :tenantId
    """)
    long countDistinctUserIdByTenantId(
            UUID tenantId
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

    @Query("""
        select du.deviceId, u.username
        from DeviceUsage du
        join User u on u.id = du.userId
        where du.tenantId = :tenantId
        and du.deviceId in :deviceIds
    """)
    List<Object[]> findDeviceUsernames(
            UUID tenantId,
            List<UUID> deviceIds
    );

    @Query("""
        select du.deviceId, pu.username
        from DeviceUsage du
        join PlatformUser pu on pu.id = du.userId
        where du.tenantId = :tenantId
        and du.deviceId in :deviceIds
    """)
    List<Object[]> findPlatformDeviceUsernames(
            UUID tenantId,
            List<UUID> deviceIds
    );
}