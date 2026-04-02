package com.IntegrityTechnologies.business_manager.security.auth.service;

import com.IntegrityTechnologies.business_manager.exception.UserNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUser;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.IntegrityTechnologies.business_manager.security.auth.config.PasswordResetProperties;
import com.IntegrityTechnologies.business_manager.security.auth.model.PasswordResetAudit;
import com.IntegrityTechnologies.business_manager.security.auth.model.PasswordResetToken;
import com.IntegrityTechnologies.business_manager.security.auth.model.UserType;
import com.IntegrityTechnologies.business_manager.security.auth.repository.PasswordResetAuditRepository;
import com.IntegrityTechnologies.business_manager.security.auth.repository.PasswordResetTokenRepository;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserRepository;

import lombok.RequiredArgsConstructor;

import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class PasswordResetService {

    private final PasswordResetProperties props;
    private final UserRepository userRepository;
    private final PasswordResetTokenRepository tokenRepo;
    private final PasswordResetAuditRepository auditRepo;
    private final PasswordEncoder passwordEncoder;
    private final AuthService authService;
    private final PlatformUserRepository platformUserRepository;

    /* =====================================================
       RESET USING IDENTITY (NO TOKEN)
       ===================================================== */

    @Transactional
    public void resetWithIdentity(
            String identifier,
            String idNumber,
            String email,
            String phone,
            String newPassword
    ) {

        User user = userRepository.findByIdentifier(identifier, TenantContext.getTenantId())
                .orElseThrow(() -> new RuntimeException("Invalid reset request"));

        try {
            validateIdentity(user, idNumber, email, phone);
            applyPasswordChange(user, newPassword);

            audit(
                    user.getId(),
                    UserType.TENANT,
                    user.getTenantId(),
                    "IDENTITY",
                    identifier,
                    "COMPLETED",
                    "Identity reset"
            );

        } catch (RuntimeException ex) {
            audit(
                    user.getId(),
                    UserType.TENANT,
                    user.getTenantId(),
                    "IDENTITY",
                    identifier,
                    "FAILED",
                    ex.getMessage()
            );
            throw ex;
        }
    }

    @Transactional
    public void forcePasswordReset(String identifier, String newPassword) {

        UUID tenantId = TenantContext.getOrNull();

        /* PLATFORM CONTEXT */

        if (tenantId == null) {

            PlatformUser user =
                    platformUserRepository
                            .findByUsernameAndDeletedFalse(identifier)
                            .orElseThrow(() -> new UserNotFoundException("User not found"));

            user.setPassword(passwordEncoder.encode(newPassword));
            user.setMustChangePassword(false);

            platformUserRepository.save(user);

            return;
        }

        /* TENANT CONTEXT */

        User user =
                userRepository
                        .findAuthUser(tenantId, identifier)
                        .orElseThrow(() -> new UserNotFoundException("User not found"));

        applyPasswordChange(user, newPassword);
    }

    /* =====================================================
       RESET USING TOKEN (EMAIL / SMS)
       ===================================================== */

    @Transactional
    public void resetWithToken(String rawToken, String newPassword) {

        String hash = DigestUtils.sha256Hex(rawToken);

        PasswordResetToken token = tokenRepo
                .findByTokenHashAndUsedFalse(hash)
                .orElseThrow(() -> new RuntimeException("Invalid or expired token"));

        if (token.getExpiresAt().isBefore(LocalDateTime.now())) {
            recordFailure(token, "Token expired");
            throw new RuntimeException("Invalid or expired token");
        }

        if (token.getAttempts() >= props.getMaxAttempts()) {
            recordFailure(token, "Too many attempts");
            throw new RuntimeException("Too many attempts");
        }

        token.incrementAttempts();
        tokenRepo.save(token);

        if (token.getUserType() == UserType.PLATFORM) {

            PlatformUser user = platformUserRepository
                    .findById(token.getUserId())
                    .orElseThrow();

            user.setPassword(passwordEncoder.encode(newPassword));
            user.setMustChangePassword(false);

            platformUserRepository.save(user);

            token.markUsed();
            tokenRepo.save(token);

            audit(
                    user.getId(),
                    UserType.PLATFORM,
                    null,
                    token.getChannel().name(),
                    user.getUsername(),
                    "COMPLETED",
                    "Platform token reset"
            );

            return;
        }

        User user = userRepository.findById(token.getUserId()).orElseThrow();

        try {
            applyPasswordChange(user, newPassword);

            token.markUsed();
            tokenRepo.save(token);

            audit(
                    user.getId(),
                    UserType.TENANT,
                    user.getTenantId(),
                    token.getChannel().name(),
                    user.getUsername(),
                    "COMPLETED",
                    "Token reset"
            );

        } catch (RuntimeException ex) {
            audit(
                    user.getId(),
                    UserType.TENANT,
                    user.getTenantId(),
                    token.getChannel().name(),
                    user.getUsername(),
                    "FAILED",
                    ex.getMessage()
            );
            throw ex;
        }
    }

    /* =====================================================
       TOKEN CREATION
       ===================================================== */

    public PasswordResetToken createToken(
            UUID userId,
            UserType userType,
            PasswordResetToken.Channel channel,
            int expiryMinutes
    ) {

        String raw = UUID.randomUUID().toString().replace("-", "");
        String otp = raw.substring(0, 6).toUpperCase();
        String hash = DigestUtils.sha256Hex(otp);

        PasswordResetToken token = PasswordResetToken.builder()
                .userId(userId)
                .userType(userType)
                .tokenHash(hash)
                .channel(channel)
                .attempts(0)
                .expiresAt(LocalDateTime.now().plusMinutes(expiryMinutes))
                .used(false)
                .build();

        tokenRepo.save(token);

        // 🔐 Return RAW OTP only (never persisted)
        token.setTokenHash(otp);
        return token;
    }

    /* =====================================================
       TOKEN VERIFICATION
       ===================================================== */

    @Transactional(readOnly = true)
    public void verifyToken(String rawToken) {

        String hash = DigestUtils.sha256Hex(rawToken);

        PasswordResetToken token = tokenRepo
                .findByTokenHashAndUsedFalse(hash)
                .orElseThrow(() -> new RuntimeException("Invalid or expired code"));

        if (token.getExpiresAt().isBefore(LocalDateTime.now())) {
            throw new RuntimeException("Invalid or expired code");
        }

        if (token.getAttempts() >= props.getMaxAttempts()) {
            throw new RuntimeException("Too many attempts");
        }

        // ⚠️ IMPORTANT:
        // Do NOT mark used
        // Do NOT increment attempts here
    }

    /* =====================================================
       INTERNAL HELPERS
       ===================================================== */

    private void applyPasswordChange(User user, String newPassword) {

        user.setPassword(passwordEncoder.encode(newPassword));

        user.setMustChangePassword(false);   // ✅ clear requirement

        userRepository.save(user);

        if (props.isInvalidateSessions()) {
            authService.logoutAllSessions(user.getId(), true);
        }
    }

    private void validateIdentity(
            User user,
            String idNumber,
            String email,
            String phone
    ) {
        boolean matched = false;

        if (email != null && user.getEmailAddresses().contains(email)) {
            matched = true;
        }

        if (idNumber != null && safeEquals(user.getIdNumber(), idNumber)) {
            matched = true;
        }

        if (phone != null && user.getPhoneNumbers().contains(phone)) {
            matched = true;
        }

        if (!matched) {
            throw new RuntimeException("Invalid reset request");
        }
    }

    private void recordFailure(PasswordResetToken token, String reason) {
        audit(
                token.getUserId(),
                token.getUserType(),
                null,
                token.getChannel().name(),
                "token",
                "FAILED",
                reason
        );
    }

    private void audit(
            UUID userId,
            UserType userType,
            UUID tenantId,
            String channel,
            String identifier,
            String status,
            String reason
    ) {
        auditRepo.save(
                PasswordResetAudit.builder()
                        .userId(userId)
                        .userType(userType)
                        .tenantId(tenantId)
                        .channel(channel)
                        .identifierUsed(mask(identifier))
                        .status(status)
                        .reason(reason)
                        .timestamp(LocalDateTime.now())
                        .build()
        );
    }

    private boolean safeEquals(String a, String b) {
        if (a == null || b == null) return false;
        return MessageDigest.isEqual(
                a.getBytes(StandardCharsets.UTF_8),
                b.getBytes(StandardCharsets.UTF_8)
        );
    }

    private String mask(String value) {
        if (value == null || value.length() < 4) return "***";
        return value.substring(0, 2) + "***" + value.substring(value.length() - 2);
    }
}