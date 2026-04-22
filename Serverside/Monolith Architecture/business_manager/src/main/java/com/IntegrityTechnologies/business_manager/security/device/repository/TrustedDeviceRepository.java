package com.IntegrityTechnologies.business_manager.security.device.repository;

import com.IntegrityTechnologies.business_manager.security.device.model.TrustedDevice;
import jakarta.persistence.LockModeType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface TrustedDeviceRepository extends JpaRepository<TrustedDevice, UUID> {

    Optional<TrustedDevice> findByTenantIdAndBranchIdAndDeviceId(
            UUID tenantId,
            UUID branchId,
            String deviceId
    );

    Optional<TrustedDevice> findByTenantIdAndBranchIdIsNullAndDeviceId(
            UUID tenantId,
            String deviceId
    );

    boolean existsByTenantIdAndBranchId(UUID tenantId, UUID branchId);
    boolean existsByTenantIdAndBranchIdIsNull(UUID tenantId);

    List<TrustedDevice> findByTenantIdAndBranchId(UUID tenantId, UUID branchId);

    Optional<TrustedDevice> findByIdAndTenantId(UUID id, UUID tenantId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("""
        select d from TrustedDevice d
        where d.tenantId=:tenantId
        and d.branchId=:branchId
    """)
    List<TrustedDevice> lockBranchDevices(
            UUID tenantId,
            UUID branchId
    );


    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("""
        select d from TrustedDevice d
        where d.tenantId=:tenantId
        and d.branchId is null
    """)
    List<TrustedDevice> lockPlatformDevices(
            UUID tenantId
    );
}