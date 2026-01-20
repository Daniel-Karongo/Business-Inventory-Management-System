package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.controller;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto.BulkSmsRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.dto.SmsRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsAccount;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsMessage;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service.SmsAccountService;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service.SmsService;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/notification/sms")
@RequiredArgsConstructor
@Tag(name = "Smss")
public class SmsController {

    private final SmsService service;
    private final SmsAccountService smsAccountService;

    @PostMapping("/send")
    public ResponseEntity<SmsMessage> send(@Valid @RequestBody SmsRequest req) {
        return ResponseEntity.ok(service.sendSms(req));
    }

    @PostMapping("/send-bulk")
    public ResponseEntity<List<SmsMessage>> sendBulk(@RequestBody BulkSmsRequest req) {
        return ResponseEntity.ok(service.sendBulk(req.getMessages()));
    }

    @GetMapping("/{id}")
    public ResponseEntity<SmsMessage> get(@PathVariable UUID id) {
        SmsMessage msg = service.getMessage(id);
        if (msg == null) return ResponseEntity.notFound().build();
        return ResponseEntity.ok(msg);
    }

    @PostMapping("/admin/sms/account")
    public ResponseEntity<SmsAccount> updateSmsAccount(
            @RequestBody SmsAccount req) {

        return ResponseEntity.ok(
                smsAccountService.saveAndActivate(req)
        );
    }
}