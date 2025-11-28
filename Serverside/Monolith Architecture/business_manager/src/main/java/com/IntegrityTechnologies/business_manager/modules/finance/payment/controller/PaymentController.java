package com.IntegrityTechnologies.business_manager.modules.finance.payment.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.service.PaymentService;

import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@Tag(name = "Payments")
@RestController
@RequestMapping("/api/payments")
@RequiredArgsConstructor
public class PaymentController {

    private final PaymentService paymentService;

    @PreAuthorize("hasAnyRole('ADMIN','CASHIER','MANAGER')")
    @PostMapping
    public ResponseEntity<PaymentResponse> makePayment(@RequestBody PaymentRequest req) {
        PaymentResponse resp = paymentService.processPayment(req);
        return ResponseEntity.ok(resp);
    }

    @PreAuthorize("hasAnyRole('ADMIN','CASHIER','MANAGER')")
    @GetMapping("/{id}")
    public ResponseEntity<PaymentResponse> getPayment(@PathVariable UUID id) {
        return ResponseEntity.ok(paymentService.getPayment(id));
    }
}