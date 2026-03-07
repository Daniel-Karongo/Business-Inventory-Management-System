package com.IntegrityTechnologies.business_manager.modules.person.function.biometric.controller;

import com.IntegrityTechnologies.business_manager.modules.person.function.biometric.model.BiometricRecord;
import com.IntegrityTechnologies.business_manager.modules.person.function.biometric.model.BiometricType;
import com.IntegrityTechnologies.business_manager.modules.person.function.biometric.service.BiometricService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.*;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Base64;
import java.util.UUID;

@Tag(name = "Biometrics")
@RestController
@RequestMapping("/api/biometric")
@RequiredArgsConstructor
@TenantUserOnly
public class BiometricController {

    private final BiometricService biometricService;
    private final UserRepository userRepository;

    @TenantSupervisorOnly
    @PostMapping("/enroll")
    @Operation(summary = "Enroll a biometric template for a user (base64 payload)")
    public ResponseEntity<BiometricRecord> enroll(
            @RequestParam UUID userId,
            @RequestParam BiometricType type,
            @RequestParam String templateBase64,
            @RequestParam(required = false) String providerId
    ) {

        byte[] template = Base64.getDecoder().decode(templateBase64);

        BiometricRecord rec =
                biometricService.enroll(userId, type, template, providerId);

        return ResponseEntity.ok(rec);
    }

    @TenantSupervisorOnly
    @DeleteMapping("/remove/{id}")
    @Operation(summary = "Remove (soft-delete) a biometric record by id")
    public ResponseEntity<Void> remove(
            @PathVariable UUID id
    ) {

        biometricService.deleteRecord(id);

        return ResponseEntity.noContent().build();
    }
}