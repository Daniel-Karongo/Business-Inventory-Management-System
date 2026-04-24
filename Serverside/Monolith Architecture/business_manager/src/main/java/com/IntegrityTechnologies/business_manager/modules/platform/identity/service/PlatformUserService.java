package com.IntegrityTechnologies.business_manager.modules.platform.identity.service;

import com.IntegrityTechnologies.business_manager.config.util.PhoneAndEmailNormalizer;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.dto.PlatformUserAuditResponse;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.dto.PlatformUserCreateRequest;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.dto.PlatformUserResponse;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.dto.PlatformUserUpdateRequest;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformRole;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUser;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUserAudit;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.mapper.PlatformUserMapper;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserRepository;
import com.IntegrityTechnologies.business_manager.security.auth.service.AuthService;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
@Transactional
@RequiredArgsConstructor
public class PlatformUserService {

    private final PlatformUserRepository repository;
    private final PlatformUserAuditRepository auditRepository;
    private final PasswordEncoder passwordEncoder;
    private final AuthService authService;

    /* =====================================================
       CREATE
    ===================================================== */

    public PlatformUserResponse createUser(
            PlatformUserCreateRequest request
    ) {

        if (repository.existsByUsername(request.getUsername())) {
            throw new IllegalStateException("Platform user already exists");
        }

        PlatformUser user = PlatformUser.builder()
                .username(request.getUsername())
                .password(passwordEncoder.encode(request.getPassword()))
                .role(PlatformRole.valueOf(request.getRole()))
                .mustChangePassword(
                        Boolean.TRUE.equals(
                                request.getMustChangePassword()
                        )
                )

                .emailAddresses(
                        PhoneAndEmailNormalizer.normalizeEmails(
                                request.getEmailAddresses() == null
                                        ? List.of()
                                        : List.copyOf(request.getEmailAddresses())
                        )
                )

                .phoneNumbers(
                        PhoneAndEmailNormalizer.normalizePhones(
                                request.getPhoneNumbers() == null
                                        ? List.of()
                                        : List.copyOf(request.getPhoneNumbers())
                        )
                )

                .idNumber(
                        request.getIdNumber()
                )
                .active(true)
                .locked(false)
                .deleted(false)
                .build();

        repository.save(user);

        audit(user, "CREATE", "Platform user created");

        return PlatformUserMapper.toResponse(user);
    }

    /* =====================================================
       LIST USERS
    ===================================================== */

    public Page<PlatformUserResponse> getUsers(Pageable pageable) {

        return repository
                .findAllByDeletedFalse(pageable)
                .map(PlatformUserMapper::toResponse);
    }

    /* =====================================================
       GET USER
    ===================================================== */

    public PlatformUserResponse getUser(UUID id) {

        PlatformUser user = repository.findDetailedById(id)
                .orElseThrow(() -> new RuntimeException("User not found"));

        return PlatformUserMapper.toResponse(user);
    }

    /* =====================================================
       UPDATE USER
    ===================================================== */

    public PlatformUserResponse updateUser(
            UUID id,
            PlatformUserUpdateRequest request
    ) {
        PlatformUser user = repository.findDetailedById(id)
                .orElseThrow(() -> new RuntimeException("User not found"));

        boolean editingSelf =
                user.getUsername()
                        .equals(SecurityUtils.currentUsername());

        if (!editingSelf) {
            ensureNotSuperuser(user);
        }

        if (request.getUsername() != null) {
            user.setUsername(request.getUsername());
        }

        if (request.getRole() != null) {
            user.setRole(PlatformRole.valueOf(request.getRole()));
        }

        if (request.getActive() != null) {
            user.setActive(request.getActive());
        }

        if (request.getLocked() != null) {
            user.setLocked(request.getLocked());
        }

        if (request.getEmailAddresses() != null) {
            user.setEmailAddresses(
                    PhoneAndEmailNormalizer.normalizeEmails(
                            List.copyOf(request.getEmailAddresses())
                    )
            );
        }

        if (request.getPhoneNumbers() != null) {
            user.setPhoneNumbers(
                    PhoneAndEmailNormalizer.normalizePhones(
                            List.copyOf(request.getPhoneNumbers())
                    )
            );
        }

        if (request.getIdNumber() != null) {
            user.setIdNumber(request.getIdNumber());
        }

        if (request.getMustChangePassword() != null) {
            user.setMustChangePassword(
                    request.getMustChangePassword()
            );
        }

        /* SELF password change only */
        if (request.getPassword() != null &&
                !request.getPassword().isBlank()) {

            if (!editingSelf) {
                throw new IllegalStateException(
                        "Only the authenticated user may change their own password"
                );
            }

            user.setPassword(
                    passwordEncoder.encode(
                            request.getPassword()
                    )
            );

            user.setMustChangePassword(false);
            authService.logoutAllSessions(
                    user.getId(),
                    true
            );

            audit(
                    user,
                    "PASSWORD_CHANGE",
                    "Password changed"
            );
        }

        repository.save(user);

        if (request.getPassword() == null || request.getPassword().isBlank()) {
            audit(user, "UPDATE", "Platform user updated");
        }

        return PlatformUserMapper.toResponse(user);
    }

    /* =====================================================
       LOCK
    ===================================================== */

    public void lockUser(UUID id) {

        PlatformUser user = repository.findDetailedById(id)
                .orElseThrow(() -> new RuntimeException("User not found"));

        ensureNotSuperuser(user);

        user.setLocked(true);

        repository.save(user);

        audit(user, "LOCK", "User locked");
    }

    /* =====================================================
       UNLOCK
    ===================================================== */

    public void unlockUser(UUID id) {

        PlatformUser user = repository.findDetailedById(id)
                .orElseThrow(() -> new RuntimeException("User not found"));

        user.setLocked(false);

        repository.save(user);

        audit(user, "UNLOCK", "User unlocked");
    }

    /* =====================================================
       DELETE
    ===================================================== */

    public void deleteUser(UUID id) {

        PlatformUser user = repository.findDetailedById(id)
                .orElseThrow(() -> new RuntimeException("User not found"));

        if (user.getUsername().equals(SecurityUtils.currentUsername())) {
            throw new IllegalStateException("Cannot delete your own account");
        }

        ensureNotSuperuser(user);

        user.setDeleted(true);

        repository.save(user);

        audit(user, "DELETE", "User soft deleted");
    }

    /* =====================================================
       AUDIT
    ===================================================== */

    public Page<PlatformUserAuditResponse> auditLogs(
            Pageable pageable
    ) {

        return auditRepository
                .findAll(pageable)
                .map(PlatformUserMapper::toAuditResponse);

    }

    /* =====================================================
       AUDIT HELPER
    ===================================================== */

    private void audit(PlatformUser user, String action, String reason) {

        auditRepository.save(
                PlatformUserAudit.builder()
                        .userId(user.getId())
                        .username(user.getUsername())
                        .action(action)
                        .reason(reason)
                        .performedBy(SecurityUtils.currentUsername())
                        .build()
        );
    }

    private void ensureNotSuperuser(PlatformUser user) {

        if (user.getRole() == PlatformRole.PLATFORM_SUPER_ADMIN) {
            throw new IllegalStateException(
                    "SUPERUSER accounts cannot be modified"
            );
        }
    }

}