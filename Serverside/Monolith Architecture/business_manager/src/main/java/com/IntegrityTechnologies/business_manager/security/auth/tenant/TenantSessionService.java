package com.IntegrityTechnologies.business_manager.security.auth.tenant;

import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.UserSession;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.repository.UserSessionRepository;
import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class TenantSessionService {

    private final UserSessionRepository repository;

    public void create(
            UUID tenantId,
            UUID branchId,
            UUID userId,
            UUID tokenId,
            AuthRequest request,
            String ipAddress,
            String deviceName
    ) {
        UserSession session = UserSession.builder()
                .tenantId(tenantId)
                .branchId(branchId)
                .userId(userId)
                .loginDate(LocalDate.now())
                .loginTime(LocalDateTime.now())
                .tokenId(tokenId)
                .autoLoggedOut(false)

                .deviceId(request.getDeviceId())
                .deviceName(deviceName)
                .browserName(request.getBrowserName())
                .osName(request.getOsName())
                .platform(request.getPlatform())
                .userAgent(request.getUserAgent())
                .ipAddress(ipAddress)

                .build();

        repository.save(session);
    }
}