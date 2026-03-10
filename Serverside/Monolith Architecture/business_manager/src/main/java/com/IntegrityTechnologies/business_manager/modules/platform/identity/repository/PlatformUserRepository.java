package com.IntegrityTechnologies.business_manager.modules.platform.identity.repository;

import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUser;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface PlatformUserRepository
        extends JpaRepository<PlatformUser, UUID> {

    Optional<PlatformUser> findByUsernameAndDeletedFalse(String username);

    boolean existsByUsername(String username);

    Page<PlatformUser> findAllByDeletedFalse(Pageable pageable);

}