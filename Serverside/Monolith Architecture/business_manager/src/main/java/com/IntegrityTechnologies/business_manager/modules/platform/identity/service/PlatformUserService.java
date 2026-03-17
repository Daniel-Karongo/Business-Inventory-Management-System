package com.IntegrityTechnologies.business_manager.modules.platform.identity.service;

import com.IntegrityTechnologies.business_manager.modules.platform.identity.dto.PlatformUserResponse;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.dto.PlatformUserUpdateRequest;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformRole;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUser;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUserAudit;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.mapper.PlatformUserMapper;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserRepository;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class PlatformUserService {

    private final PlatformUserRepository repository;
    private final PlatformUserAuditRepository auditRepository;
    private final PasswordEncoder passwordEncoder;

    /* =====================================================
       CREATE
    ===================================================== */

    public PlatformUserResponse createUser(
            String username,
            String password,
            String role
    ) {

        if (repository.existsByUsername(username)) {
            throw new IllegalStateException("Platform user already exists");
        }

        PlatformUser user = PlatformUser.builder()
                .username(username)
                .password(passwordEncoder.encode(password))
                .role(PlatformRole.valueOf(role))
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

        PlatformUser user = repository.findById(id)
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

        PlatformUser user = repository.findById(id)
                .orElseThrow(() -> new RuntimeException("User not found"));

        ensureNotSuperuser(user);

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
            user.setEmailAddresses(request.getEmailAddresses());
        }

        if (request.getPhoneNumbers() != null) {
            user.setPhoneNumbers(request.getPhoneNumbers());
        }

        if (request.getIdNumber() != null) {
            user.setIdNumber(request.getIdNumber());
        }

        repository.save(user);

        audit(user, "UPDATE", "Platform user updated");

        return PlatformUserMapper.toResponse(user);
    }

    /* =====================================================
       LOCK
    ===================================================== */

    public void lockUser(UUID id) {

        PlatformUser user = repository.findById(id)
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

        PlatformUser user = repository.findById(id)
                .orElseThrow(() -> new RuntimeException("User not found"));

        user.setLocked(false);

        repository.save(user);

        audit(user, "UNLOCK", "User unlocked");
    }

    /* =====================================================
       DELETE
    ===================================================== */

    public void deleteUser(UUID id) {

        PlatformUser user = repository.findById(id)
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

    public Page<PlatformUserAudit> auditLogs(Pageable pageable) {
        return auditRepository.findAll(pageable);
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