package com.IntegrityTechnologies.business_manager.modules.finance.payment.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.service.PaymentService;

import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@Tag(name = "Payments")
@RestController
@RequestMapping("/api/payments")
@RequiredArgsConstructor
public class PaymentController {

    private final PaymentService paymentService;

    @PostMapping
    public ResponseEntity<PaymentResponse> makePayment(@RequestBody PaymentRequest req) {
        return ResponseEntity.ok(paymentService.processPayment(req));
    }

    @GetMapping("/{id}")
    public ResponseEntity<PaymentResponse> getPayment(@PathVariable UUID id) {
        return ResponseEntity.ok(paymentService.getPayment(id));
    }

    /** NEW — List payments */
    @GetMapping
    public ResponseEntity<Page<PaymentResponse>> listPayments(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size,
            @RequestParam(required = false) String method,
            @RequestParam(required = false) String status,
            @RequestParam(required = false) UUID saleId
    ) {
        return ResponseEntity.ok(
                paymentService.listPayments(page, size, method, status, saleId));
    }

    /** NEW — Refund payment */
    @PostMapping("/{id}/refund")
    public ResponseEntity<PaymentResponse> refundPayment(@PathVariable UUID id) {
        return ResponseEntity.ok(paymentService.refundPayment(id));
    }

    /** NEW — Reconciliation */
    @GetMapping("/reconcile")
    public ResponseEntity<Object> reconcilePayments(
            @RequestParam String from,
            @RequestParam String to
    ) {
        return ResponseEntity.ok(paymentService.reconcile(from, to));
    }
}