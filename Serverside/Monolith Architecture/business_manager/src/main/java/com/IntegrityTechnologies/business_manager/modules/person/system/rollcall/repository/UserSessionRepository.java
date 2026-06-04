package com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.repository;

import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.UserSession;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface UserSessionRepository extends JpaRepository<UserSession, UUID> {
    Optional<UserSession> findFirstByUserIdAndLogoutTimeIsNull(UUID userId);
    Optional<UserSession> findByUserIdAndTokenIdAndLogoutTimeIsNull(
            UUID userId,
            UUID tokenId
    );
    List<UserSession> findAllByUserIdAndLogoutTimeIsNullOrderByLoginTimeDesc(
            UUID userId
    );
    long countByUserIdAndLogoutTimeIsNull(
            UUID userId
    );
    boolean existsByUserIdAndBranchIdAndLogoutTimeIsNull(UUID userId, UUID branchId);
    Optional<UserSession> findByTokenIdAndLogoutTimeIsNull(UUID tokenId);
    List<UserSession> findAllByUserIdAndLogoutTimeIsNull(UUID userId);
}