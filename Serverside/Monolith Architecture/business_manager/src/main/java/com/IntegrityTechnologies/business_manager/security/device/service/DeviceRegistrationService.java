package com.IntegrityTechnologies.business_manager.security.device.service;

import com.IntegrityTechnologies.business_manager.security.device.model.DeviceApprovalStatus;
import com.IntegrityTechnologies.business_manager.security.device.model.DeviceRegistrationContext;
import com.IntegrityTechnologies.business_manager.security.device.model.TrustedDevice;
import com.IntegrityTechnologies.business_manager.security.device.repository.TrustedDeviceRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class DeviceRegistrationService {

    private final TrustedDeviceRepository repository;

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public TrustedDevice registerPending(
            UUID tenantId,
            UUID branchId,
            String deviceId,
            boolean autoApproveFirstDevice,

            String deviceName,
            String browserName,
            String osName,
            String platform,
            String userAgent,
            String ipAddress
    ) {

        TrustedDevice device =
                TrustedDevice.builder()
                        .tenantId(tenantId)
                        .branchId(branchId)
                        .deviceId(deviceId)

                        .deviceName(deviceName)

                        .browserName(browserName)
                        .osName(osName)
                        .platform(platform)
                        .userAgent(userAgent)
                        .ipAddress(ipAddress)

                        .status(
                                autoApproveFirstDevice
                                        ? DeviceApprovalStatus.APPROVED
                                        : DeviceApprovalStatus.PENDING
                        )
                        .build();

        return repository.save(device);
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public TrustedDevice registerPending(
            DeviceRegistrationContext ctx,
            boolean autoApproveFirstDevice
    ) {

        return registerPending(
                ctx.getTenantId(),
                ctx.getBranchId(),
                ctx.getDeviceId(),
                autoApproveFirstDevice,

                ctx.getDeviceName(),

                ctx.getBrowserName(),
                ctx.getOsName(),
                ctx.getPlatform(),
                ctx.getUserAgent(),

                ctx.getIpAddress()
        );
    }
}