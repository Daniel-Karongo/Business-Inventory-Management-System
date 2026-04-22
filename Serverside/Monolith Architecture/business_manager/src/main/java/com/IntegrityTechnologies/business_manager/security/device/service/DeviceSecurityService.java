package com.IntegrityTechnologies.business_manager.security.device.service;

import com.IntegrityTechnologies.business_manager.exception.AppSecurityException;
import com.IntegrityTechnologies.business_manager.security.audit.service.LoginAuditService;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.security.biometric.repository.UserBiometricRepository;
import com.IntegrityTechnologies.business_manager.security.device.model.DeviceApprovalStatus;
import com.IntegrityTechnologies.business_manager.security.device.model.DeviceRegistrationContext;
import com.IntegrityTechnologies.business_manager.security.device.model.TrustedDevice;
import com.IntegrityTechnologies.business_manager.security.device.repository.TrustedDeviceRepository;
import com.IntegrityTechnologies.business_manager.security.model.SecurityErrorCode;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class DeviceSecurityService {

    private final TrustedDeviceRepository repository;
    private final UserBiometricRepository biometricRepository;
    private final DeviceRegistrationService deviceRegistrationService;
    private final DeviceUsageService deviceUsageService;
    private final LoginAuditService loginAuditService;

    @Transactional
    public void validate(
            UUID tenantId,
            UUID branchId,
            String deviceId,
            UUID attemptingUserId,
            AuthRequest request,
            String ipAddress
    ) {

        var deviceOpt =
                branchId == null
                        ? repository.findByTenantIdAndBranchIdIsNullAndDeviceId(
                        tenantId,
                        deviceId
                )
                        : repository.findByTenantIdAndBranchIdAndDeviceId(
                        tenantId,
                        branchId,
                        deviceId
                );

        /* =========================
           FIRST TIME DEVICE
        ========================= */

        if (deviceOpt.isEmpty()) {

            if (branchId == null) {
                repository.lockPlatformDevices(tenantId);
            } else {
                repository.lockBranchDevices(
                        tenantId,
                        branchId
                );
            }

            boolean firstDevice =
                    branchId == null
                            ? !repository.existsByTenantIdAndBranchIdIsNull(
                            tenantId
                    )
                            : !repository.existsByTenantIdAndBranchId(
                            tenantId,
                            branchId
                    );

            DeviceRegistrationContext ctx =
                    DeviceRegistrationContext.builder()
                            .tenantId(tenantId)
                            .branchId(branchId)
                            .deviceId(deviceId)
                            .deviceName(buildFriendlyDeviceName(request))
                            .browserName(request.getBrowserName())
                            .osName(request.getOsName())
                            .platform(request.getPlatform())
                            .userAgent(request.getUserAgent())
                            .ipAddress(ipAddress)
                            .build();

            TrustedDevice savedDevice =
                    deviceRegistrationService.registerPending(ctx, firstDevice);

            if (!firstDevice) {

                if (attemptingUserId != null) {
                    deviceUsageService.record(
                            savedDevice.getId(),
                            attemptingUserId
                    );
                }

                // record attempt in existing audit table
                loginAuditService.log(
                        tenantId,
                        attemptingUserId,
                        branchId,
                        deviceId,
                        null,
                        null,
                        null,
                        null,
                        "BLOCKED",
                        "DEVICE_PENDING_APPROVAL"
                );

                throw new AppSecurityException(
                        SecurityErrorCode.DEVICE_PENDING_APPROVAL,
                        "Device pending approval"
                );
            }

            return;
        }

        TrustedDevice device = deviceOpt.get();

        /* =========================
           REJECTED
        ========================= */

        if (device.getStatus() == DeviceApprovalStatus.REJECTED) {

            loginAuditService.log(
                    tenantId,
                    attemptingUserId,
                    branchId,
                    deviceId,
                    null,
                    null,
                    null,
                    null,
                    "BLOCKED",
                    "DEVICE_REJECTED"
            );

            throw new AppSecurityException(
                    SecurityErrorCode.DEVICE_NOT_APPROVED,
                    "Device rejected"
            );
        }

        /* =========================
           STILL PENDING
        ========================= */

        if (device.getStatus() == DeviceApprovalStatus.PENDING) {

            loginAuditService.log(
                    tenantId,
                    attemptingUserId,
                    branchId,
                    deviceId,
                    null,
                    null,
                    null,
                    null,
                    "BLOCKED",
                    "DEVICE_PENDING_APPROVAL"
            );

            throw new AppSecurityException(
                    SecurityErrorCode.DEVICE_NOT_APPROVED,
                    "Device pending approval"
            );
        }

        /* =========================
           APPROVED
        ========================= */

        device.setLastSeenAt(LocalDateTime.now());
        repository.save(device);
    }


    public void validate(
            UUID tenantId,
            UUID branchId,
            String deviceId,
            UUID attemptingUserId
    ) {

        validate(
                tenantId,
                branchId,
                deviceId,
                attemptingUserId,
                new AuthRequest(),
                null
        );
    }

    @Value("${security.device.max-per-user:3}")
    private int maxDevicesPerUser;


    public void enforceDeviceLimit(
            UUID tenantId,
            UUID userId
    ) {

        long count =
                biometricRepository
                        .countByTenantIdAndUserIdAndDeletedFalse(
                                tenantId,
                                userId
                        );

        if (count >= maxDevicesPerUser) {

            throw new AppSecurityException(
                    SecurityErrorCode.DEVICE_LIMIT_REACHED,
                    "Maximum registered devices reached"
            );
        }
    }


    public TrustedDevice getByDeviceId(
            UUID tenantId,
            UUID branchId,
            String deviceId
    ) {

        if (branchId == null) {
            return repository
                    .findByTenantIdAndBranchIdIsNullAndDeviceId(
                            tenantId,
                            deviceId
                    )
                    .orElseThrow();
        }

        return repository
                .findByTenantIdAndBranchIdAndDeviceId(
                        tenantId,
                        branchId,
                        deviceId
                )
                .orElseThrow();
    }

    private String buildFriendlyDeviceName(AuthRequest request) {

        if (request.getBrowserName() != null &&
                request.getOsName() != null) {

            return request.getBrowserName()
                    + " on "
                    + request.getOsName();
        }

        if (request.getUserAgent() != null) {
            return "Browser Device";
        }
        return "Unidentified Device";
    }
}