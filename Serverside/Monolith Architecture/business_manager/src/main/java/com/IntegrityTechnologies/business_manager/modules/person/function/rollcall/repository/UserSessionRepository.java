package com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository;

import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.UserSession;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface UserSessionRepository extends JpaRepository<UserSession, UUID> {

    // Active sessions for a user today
    List<UserSession> findByUserIdAndLoginDateAndLogoutTimeIsNull(
            UUID userId,
            LocalDate loginDate
    );

    // All active sessions (used for token expiry)
    List<UserSession> findByLogoutTimeIsNull();

    // Resolve session from token
    Optional<UserSession> findByTokenId(UUID tokenId);

    // Active session by token
    Optional<UserSession> findByTokenIdAndLogoutTimeIsNull(UUID tokenId);

    // For password reset / force logout
    List<UserSession> findByUserIdAndLogoutTimeIsNull(UUID userId);

    // 🔹 Active sessions (not logged out yet)
    List<UserSession> findAllByLogoutTimeIsNull();

    // 🔹 All sessions for user on a given day
    List<UserSession> findAllByUserIdAndLoginDate(UUID userId, LocalDate loginDate);

    // 🔹 Active sessions for a user (used for forced logout, password change)
    List<UserSession> findAllByUserIdAndLogoutTimeIsNull(UUID userId);

    // 🔹 Latest active session (optional utility)
    Optional<UserSession> findTopByUserIdAndLogoutTimeIsNullOrderByLoginTimeDesc(UUID userId);
}