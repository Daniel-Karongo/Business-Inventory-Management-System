package com.IntegrityTechnologies.business_manager.security.biometric.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.security.biometric.dto.BiometricStatsDTO;
import com.IntegrityTechnologies.business_manager.security.biometric.dto.UserBiometricDTO;
import com.IntegrityTechnologies.business_manager.security.biometric.service.UserBiometricService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/admin/biometrics")
@RequiredArgsConstructor
@TenantManagerOnly
public class AdminBiometricController {

    private final UserBiometricService service;

    @GetMapping("/user/{userId}")
    public ResponseEntity<List<UserBiometricDTO>> userBiometrics(
            @PathVariable UUID userId
    ){
        return ResponseEntity.ok(
                service.listForAdminUser(userId)
        );
    }

    @GetMapping("/stats")
    public ResponseEntity<BiometricStatsDTO> stats(){
        return ResponseEntity.ok(
                service.stats()
        );
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> adminDelete(
            @PathVariable UUID id,
            @RequestParam(defaultValue="false") boolean hard
    ){
        service.adminDelete(
                id,
                hard
        );

        return ResponseEntity.ok().build();
    }
}