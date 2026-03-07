package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.controller;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service.SmsDeliveryReportService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/api/notification/sms/dlr")
@RequiredArgsConstructor
@TenantUserOnly
public class SmsDlrController {

    private final SmsDeliveryReportService service;

    @PostMapping
    public ResponseEntity<Void> receive(@RequestParam Map<String, String> payload) {
        service.handleDeliveryReport(payload);
        return ResponseEntity.ok().build();
    }
}