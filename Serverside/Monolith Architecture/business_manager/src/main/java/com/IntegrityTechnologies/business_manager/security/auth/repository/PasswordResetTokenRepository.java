package com.IntegrityTechnologies.business_manager.security.auth.repository;

import com.IntegrityTechnologies.business_manager.security.auth.model.PasswordResetToken;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface PasswordResetTokenRepository
        extends JpaRepository<PasswordResetToken, UUID> {

    Optional<PasswordResetToken> findByTokenHashAndUsedFalse(String tokenHash);
}