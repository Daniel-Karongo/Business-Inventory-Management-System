package com.IntegrityTechnologies.business_manager.modules.platform.identity.repository;

import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUserSession;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface PlatformUserSessionRepository
        extends JpaRepository<PlatformUserSession, UUID> {

    Optional<PlatformUserSession> findByTokenIdAndLogoutTimeIsNull(UUID tokenId);

    List<PlatformUserSession> findAllByUserIdAndLogoutTimeIsNull(UUID userId);

    long countByUserIdAndLogoutTimeIsNull(UUID id);
}