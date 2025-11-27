package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.controller;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto.BulkSmsRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto.SmsRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsMessage;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service.SmsService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/notification/sms")
@RequiredArgsConstructor
public class SmsController {

    private final SmsService service;

    @PreAuthorize("hasAnyRole('ADMIN','MANAGER')")
    @PostMapping("/send")
    public ResponseEntity<SmsMessage> send(@RequestBody SmsRequest req) {
        return ResponseEntity.ok(service.sendSms(req));
    }

    @PreAuthorize("hasAnyRole('ADMIN','MANAGER')")
    @PostMapping("/send-bulk")
    public ResponseEntity<List<SmsMessage>> sendBulk(@RequestBody BulkSmsRequest req) {
        return ResponseEntity.ok(service.sendBulk(req.getMessages()));
    }

    @PreAuthorize("hasAnyRole('ADMIN','MANAGER','CASHIER')")
    @GetMapping("/{id}")
    public ResponseEntity<SmsMessage> get(@PathVariable UUID id) {
        SmsMessage msg = service.getMessage(id);
        if (msg == null) return ResponseEntity.notFound().build();
        return ResponseEntity.ok(msg);
    }
}