package com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository;

import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.UserSession;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface UserSessionRepository extends JpaRepository<UserSession, UUID> {

    Optional<UserSession> findTopByUserIdAndLogoutTimeIsNullOrderByLoginTimeDesc(UUID userId);

    List<UserSession> findAllByLogoutTimeIsNull();
}
