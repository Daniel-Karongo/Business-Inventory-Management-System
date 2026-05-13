package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.controller;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto.BulkSmsRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto.SmsRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsMessage;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service.SmsService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/notification/sms")
@RequiredArgsConstructor
@Tag(name = "SMS")
@TenantUserOnly
public class SmsController {

    private final SmsService service;

    @TenantManagerOnly
    @PostMapping("/branch/{branchId}/send")
    public ResponseEntity<SmsMessage> send(
            @PathVariable UUID branchId,
            @Valid @RequestBody SmsRequest req
    ) {

        return ResponseEntity.ok(
                service.sendSms(
                        branchId,
                        req
                )
        );
    }

    @TenantManagerOnly
    @PostMapping("/branch/{branchId}/send-bulk")
    public ResponseEntity<List<SmsMessage>> sendBulk(
            @PathVariable UUID branchId,
            @RequestBody BulkSmsRequest req
    ) {

        return ResponseEntity.ok(
                service.sendBulk(
                        branchId,
                        req.getMessages()
                )
        );
    }

    @GetMapping("/branch/{branchId}/{id}")
    public ResponseEntity<SmsMessage> get(
            @PathVariable UUID branchId,
            @PathVariable UUID id
    ) {

        return ResponseEntity.ok(
                service.getMessage(
                        branchId,
                        id
                )
        );
    }
}