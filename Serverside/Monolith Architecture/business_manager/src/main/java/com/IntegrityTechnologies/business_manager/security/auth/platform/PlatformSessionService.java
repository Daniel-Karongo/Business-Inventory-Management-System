package com.IntegrityTechnologies.business_manager.security.auth.platform;

import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUserSession;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserSessionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class PlatformSessionService {

    private final PlatformUserSessionRepository repository;

    public void create(UUID userId, UUID tokenId) {

        PlatformUserSession session = PlatformUserSession.builder()
                .userId(userId)
                .tokenId(tokenId)
                .loginDate(LocalDate.now())
                .loginTime(LocalDateTime.now())
                .autoLoggedOut(false)
                .build();

        repository.save(session);
    }
}