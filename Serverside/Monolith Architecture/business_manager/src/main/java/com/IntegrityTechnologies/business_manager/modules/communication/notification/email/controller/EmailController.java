package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.controller;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.dto.EmailRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model.EmailMessage;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.repository.EmailMessageRepository;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.service.EmailService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/notification/email")
@RequiredArgsConstructor
public class EmailController {

    private final EmailService service;
    private final EmailMessageRepository repo;

    @PostMapping("/send")
    public ResponseEntity<EmailMessage> send(
            @Valid @RequestBody EmailRequest request
    ) {
        return ResponseEntity.ok(service.send(request));
    }

    @GetMapping("/{id}")
    public ResponseEntity<EmailMessage> get(@PathVariable UUID id) {
        return repo.findById(id)
                .map(ResponseEntity::ok)
                .orElse(ResponseEntity.notFound().build());
    }
}