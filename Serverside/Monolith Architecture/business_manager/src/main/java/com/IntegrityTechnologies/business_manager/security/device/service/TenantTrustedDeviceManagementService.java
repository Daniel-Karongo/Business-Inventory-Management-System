package com.IntegrityTechnologies.business_manager.security.device.service;

import com.IntegrityTechnologies.business_manager.exception.AppSecurityException;
import com.IntegrityTechnologies.business_manager.security.audit.model.LoginAudit;
import com.IntegrityTechnologies.business_manager.security.audit.repository.LoginAuditRepository;
import com.IntegrityTechnologies.business_manager.security.device.dto.DeviceStatsDTO;
import com.IntegrityTechnologies.business_manager.security.device.dto.TrustedDeviceDTO;
import com.IntegrityTechnologies.business_manager.security.device.model.DeviceApprovalStatus;
import com.IntegrityTechnologies.business_manager.security.device.model.TrustedDevice;
import com.IntegrityTechnologies.business_manager.security.device.repository.DeviceUsageRepository;
import com.IntegrityTechnologies.business_manager.security.device.repository.TrustedDeviceRepository;
import com.IntegrityTechnologies.business_manager.security.model.SecurityErrorCode;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

@Service
@RequiredArgsConstructor
public class TenantTrustedDeviceManagementService {

    private final TrustedDeviceRepository repository;
    private final DeviceUsageRepository usageRepository;
    private final DeviceApprovalAuditService approvalAuditService;
    private final LoginAuditRepository loginAuditRepository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    public List<TrustedDeviceDTO> list(UUID branchId) {

        List<TrustedDevice> devices =
                branchId == null
                        ? repository.findByTenantIdAndBranchIdIsNull(
                        tenantId()
                )
                        : repository.findByTenantIdAndBranchId(
                        tenantId(),
                        branchId
                );

        return enrich(devices);
    }

    public List<TrustedDeviceDTO> pendingDevices() {

        return enrich(
                repository.findByTenantIdAndStatus(
                        tenantId(),
                        DeviceApprovalStatus.PENDING
                )
        );
    }

    public List<TrustedDeviceDTO> listAllAccessible() {

        return enrich(
                repository.findAllByTenantId(
                        tenantId()
                )
        );
    }

    private List<TrustedDeviceDTO> enrich(
            List<TrustedDevice> devices
    ) {

        if (devices.isEmpty()) {
            return List.of();
        }

        List<UUID> ids =
                devices.stream()
                        .map(TrustedDevice::getId)
                        .toList();

        var rows =
                usageRepository.findDeviceUsernames(
                        tenantId(),
                        ids
                );

        Map<UUID,List<String>> usersByDevice =
                new HashMap<>();

        for (Object[] row : rows) {

            UUID deviceId =
                    (UUID) row[0];

            String username =
                    (String) row[1];

            usersByDevice
                    .computeIfAbsent(
                            deviceId,
                            k -> new ArrayList<>()
                    )
                    .add(username);
        }

        Map<String,List<LoginAudit>> attemptsByFingerprint =
                new HashMap<>();

        for(TrustedDevice d : devices){

            attemptsByFingerprint.put(
                    d.getDeviceId(),
                    loginAuditRepository.findPendingAttempts(
                            tenantId(),
                            d.getDeviceId()
                    )
            );

        }

        return devices.stream()
                .map(d ->
                        TrustedDeviceDTO.builder()
                                .id(d.getId())
                                .branchId(d.getBranchId())

                                .deviceName(d.getDeviceName())

                                .browserName(d.getBrowserName())
                                .osName(d.getOsName())
                                .platform(d.getPlatform())
                                .ipAddress(d.getIpAddress())
                                .userAgent(d.getUserAgent())

                                .deviceId(d.getDeviceId())
                                .status(d.getStatus().name())

                                .firstSeenAt(d.getFirstSeenAt())
                                .lastSeenAt(d.getLastSeenAt())

                                .pendingAttemptCount(
                                        attemptsByFingerprint
                                                .getOrDefault(
                                                        d.getDeviceId(),
                                                        List.of()
                                                )
                                                .size()
                                )

                                .attemptedByUserIds(
                                        attemptsByFingerprint
                                                .getOrDefault(
                                                        d.getDeviceId(),
                                                        List.of()
                                                )
                                                .stream()
                                                .map(LoginAudit::getUserId)
                                                .distinct()
                                                .toList()
                                )

                                .usedByUsernames(
                                        usersByDevice.getOrDefault(
                                                d.getId(),
                                                List.of()
                                        )
                                )
                                .build()
                )
                .toList();
    }

    @Transactional
    public void approve(UUID id,String reason){

        var d = repository.findByIdAndTenantId(
                                id,
                                tenantId()
                        ).orElseThrow();

        if(d.getStatus()==DeviceApprovalStatus.APPROVED){
            throw new AppSecurityException(
                    SecurityErrorCode.INVALID_REQUEST,
                    "Device already approved"
            );
        }

        d.setStatus(DeviceApprovalStatus.APPROVED);

        repository.save(d);

        approvalAuditService.log(
                d.getBranchId(),
                d.getId(),
                "APPROVED",
                reason
        );
    }

    @Transactional
    public void reject(UUID id,String reason){

        var d =
                repository
                        .findByIdAndTenantId(
                                id,
                                tenantId()
                        )
                        .orElseThrow();

        if(d.getStatus()==DeviceApprovalStatus.REJECTED){
            throw new AppSecurityException(
                    SecurityErrorCode.INVALID_REQUEST,
                    "Device already rejected"
            );
        }

        d.setStatus(
                DeviceApprovalStatus.REJECTED
        );

        repository.save(d);

        approvalAuditService.log(
                d.getBranchId(),
                d.getId(),
                "REJECTED",
                reason
        );
    }

    public void rename(
            UUID id,
            String name
    ){
        if(name==null || name.isBlank()){
            throw new AppSecurityException(
                    SecurityErrorCode.INVALID_REQUEST,
                    "Device name required"
            );
        }

        var d = repository.findByIdAndTenantId(
                id,
                tenantId()
        ).orElseThrow();

        d.setDeviceName(name.trim());

        repository.save(d);

        approvalAuditService.log(
                d.getBranchId(),
                d.getId(),
                "RENAMED",
                "Tenant device renamed"
        );
    }

    public DeviceStatsDTO stats(){

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
                        usageRepository.countDistinctDevicesByTenantId(
                                tenantId()
                        )
                )
                .build();
    }

}