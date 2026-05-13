package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.controller;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.dto.EmailRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model.EmailMessage;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.service.EmailService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/notification/email")
@RequiredArgsConstructor
@TenantUserOnly
public class EmailController {

    private final EmailService service;

    @TenantManagerOnly
    @PostMapping("/branch/{branchId}/send")
    public ResponseEntity<EmailMessage> send(
            @PathVariable UUID branchId,
            @Valid @RequestBody EmailRequest request
    ) {

        return ResponseEntity.ok(
                service.send(branchId, request)
        );
    }

    @GetMapping("/branch/{branchId}/{id}")
    public ResponseEntity<EmailMessage> get(
            @PathVariable UUID branchId,
            @PathVariable UUID id
    ) {

        return ResponseEntity.ok(
                service.get(branchId, id)
        );
    }
}