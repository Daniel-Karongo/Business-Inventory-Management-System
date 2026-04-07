package com.IntegrityTechnologies.business_manager.security.biometric.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformUserOrTenantManager;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import com.IntegrityTechnologies.business_manager.security.auth.util.JwtUtil;
import com.IntegrityTechnologies.business_manager.security.biometric.dto.UserBiometricDTO;
import com.IntegrityTechnologies.business_manager.security.biometric.service.UserBiometricService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/biometrics")
@RequiredArgsConstructor
@TenantUserOnly
public class UserBiometricController {

    private final UserBiometricService service;
    private final JwtUtil jwtUtil;

    /* ================= MY DEVICES ================= */

    @GetMapping
    public ResponseEntity<List<UserBiometricDTO>> myDevices(
            @CookieValue("access_token") String token
    ) {
        UUID userId = jwtUtil.extractUserId(token);
        return ResponseEntity.ok(service.listForUser(userId));
    }

    /* ================= RENAME ================= */

    @PutMapping("/{id}/rename")
    public ResponseEntity<Void> rename(
            @PathVariable UUID id,
            @RequestParam String name,
            @CookieValue("access_token") String token
    ) {
        UUID userId = jwtUtil.extractUserId(token);
        service.rename(id, userId, name);
        return ResponseEntity.ok().build();
    }

    /* ================= DELETE ================= */

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(
            @PathVariable UUID id,
            @RequestParam(defaultValue = "false") boolean hard,
            @CookieValue("access_token") String token
    ) {
        UUID userId = jwtUtil.extractUserId(token);
        service.delete(id, userId, hard);
        return ResponseEntity.ok().build();
    }

    /* ================= ADMIN ================= */

    @DeleteMapping("/admin/{id}")
    @PlatformUserOrTenantManager
    public ResponseEntity<Void> adminDelete(
            @PathVariable UUID id,
            @RequestParam(defaultValue = "false") boolean hard
    ) {
        service.adminDelete(id, hard);
        return ResponseEntity.ok().build();
    }
}