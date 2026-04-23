package com.IntegrityTechnologies.business_manager.security.device.repository;

import com.IntegrityTechnologies.business_manager.security.device.model.DeviceApprovalStatus;
import com.IntegrityTechnologies.business_manager.security.device.model.TrustedDevice;
import org.springframework.data.jpa.repository.JpaRepository;
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

    long countByTenantIdAndBranchIdAndStatus(
            UUID tenantId,
            UUID branchId,
            DeviceApprovalStatus status
    );

    List<TrustedDevice> findByTenantIdAndStatus(
            UUID tenantId,
            DeviceApprovalStatus status
    );

    long countByTenantIdAndBranchIdIsNullAndStatus(
            UUID tenantId,
            DeviceApprovalStatus status
    );

    long countByTenantIdAndStatus(
            UUID tenantId,
            DeviceApprovalStatus status
    );

    List<TrustedDevice> findByTenantIdAndBranchId(UUID tenantId, UUID branchId);
    List<TrustedDevice> findByTenantIdAndBranchIdIsNull(UUID tenantId);

    Optional<TrustedDevice> findByIdAndTenantId(UUID id, UUID tenantId);
}