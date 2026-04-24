package com.IntegrityTechnologies.business_manager.security.device.service;

import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserRepository;
import com.IntegrityTechnologies.business_manager.security.audit.repository.LoginAuditRepository;
import com.IntegrityTechnologies.business_manager.security.device.dto.DeviceApprovalAuditDTO;
import com.IntegrityTechnologies.business_manager.security.device.dto.DeviceAttemptDTO;
import com.IntegrityTechnologies.business_manager.security.device.model.DeviceApprovalAudit;
import com.IntegrityTechnologies.business_manager.security.device.model.TrustedDevice;
import com.IntegrityTechnologies.business_manager.security.device.repository.DeviceApprovalAuditRepository;
import com.IntegrityTechnologies.business_manager.security.device.repository.TrustedDeviceRepository;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class DeviceApprovalAuditService {

    private final DeviceApprovalAuditRepository repo;
    private final TrustedDeviceRepository deviceRepository;
    private final UserRepository userRepository;
    private final PlatformUserRepository platformUserRepository;
    private final LoginAuditRepository loginAuditRepository;

    public void log(
            java.util.UUID deviceId,
            String action,
            String reason
    ) {
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
    ) {
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
                .map(a -> {

                    String username =
                            userRepository.findById(
                                            a.getActedByUserId()
                                    ).map(u -> u.getUsername())
                                    .orElseGet(() ->
                                            platformUserRepository
                                                    .findById(a.getActedByUserId())
                                                    .map(p -> p.getUsername())
                                                    .orElse("SYSTEM")
                                    );

                    return DeviceApprovalAuditDTO.builder()
                            .actedByUserId(a.getActedByUserId())
                            .actedByUsername(username)
                            .action(a.getAction())
                            .reason(a.getReason())
                            .actedAt(a.getActedAt())
                            .build();
                })
                .toList();
    }

    public List<DeviceAttemptDTO> pendingAttempts(
            UUID deviceId
    ) {

        TrustedDevice device =
                deviceRepository
                        .findByIdAndTenantId(
                                deviceId,
                                TenantContext.getTenantId()
                        )
                        .orElseThrow();

        return loginAuditRepository
                .findPendingAttempts(
                        TenantContext.getTenantId(),
                        device.getDeviceId()
                )
                .stream()
                .map(a -> {

                    String username =
                            userRepository.findById(
                                            a.getUserId()
                                    ).map(u -> u.getUsername())
                                    .orElse("UNKNOWN");

                    return DeviceAttemptDTO.builder()
                            .attemptedUserId(
                                    a.getUserId()
                            )
                            .attemptedUsername(
                                    username
                            )
                            .reason(
                                    a.getReason()
                            )
                            .attemptedAt(
                                    a.getTimestamp()
                            )
                            .build();

                })
                .toList();
    }
}