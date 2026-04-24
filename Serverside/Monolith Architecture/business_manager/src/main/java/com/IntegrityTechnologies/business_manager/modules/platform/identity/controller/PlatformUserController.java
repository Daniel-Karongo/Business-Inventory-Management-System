package com.IntegrityTechnologies.business_manager.modules.platform.identity.controller;

import com.IntegrityTechnologies.business_manager.config.response.PageWrapper;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.dto.PlatformUserAuditResponse;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.dto.PlatformUserCreateRequest;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.dto.PlatformUserResponse;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.dto.PlatformUserUpdateRequest;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.service.PlatformUserService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformSuperAdminOnly;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/platform/users")
@RequiredArgsConstructor
public class PlatformUserController {

    private final PlatformUserService service;

    /* =====================================================
       CREATE (SUPER ADMIN)
    ===================================================== */

    @PostMapping
    @PlatformSuperAdminOnly
    public PlatformUserResponse create(
            @RequestBody PlatformUserCreateRequest request
    ) {
        return service.createUser(request);
    }

    /* =====================================================
       LIST USERS (SUPER ADMIN)
    ===================================================== */

    @GetMapping
    @PlatformSuperAdminOnly
    public PageWrapper<PlatformUserResponse> users(
            Pageable pageable
    ) {

        return new PageWrapper<>(
                service.getUsers(pageable)
        );
    }

    /* =====================================================
       GET USER
    ===================================================== */

    @GetMapping("/{id}")
    @PlatformAdminOnly
    public PlatformUserResponse getUser(
            @PathVariable UUID id
    ) {

        return service.getUser(id);
    }

    /* =====================================================
       UPDATE USER
    ===================================================== */

    @PutMapping("/{id}")
    @PlatformSuperAdminOnly
    public PlatformUserResponse update(
            @PathVariable UUID id,
            @RequestBody PlatformUserUpdateRequest request
    ) {

        return service.updateUser(id, request);
    }

    /* =====================================================
       LOCK
    ===================================================== */

    @PatchMapping("/{id}/lock")
    @PlatformSuperAdminOnly
    public void lock(@PathVariable UUID id) {
        service.lockUser(id);
    }

    /* =====================================================
       UNLOCK
    ===================================================== */

    @PatchMapping("/{id}/unlock")
    @PlatformSuperAdminOnly
    public void unlock(@PathVariable UUID id) {
        service.unlockUser(id);
    }

    /* =====================================================
       DELETE
    ===================================================== */

    @DeleteMapping("/{id}")
    @PlatformSuperAdminOnly
    public void delete(@PathVariable UUID id) {
        service.deleteUser(id);
    }

    /* =====================================================
       DELETE
    ===================================================== */
    @GetMapping("/audit")
    @PlatformSuperAdminOnly
    public PageWrapper<PlatformUserAuditResponse> audit(
            Pageable pageable
    ) {
        return new PageWrapper<>(
                service.auditLogs(pageable)
        );
    }
}