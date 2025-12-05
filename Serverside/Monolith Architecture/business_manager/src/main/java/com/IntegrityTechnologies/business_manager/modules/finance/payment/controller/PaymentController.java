package com.IntegrityTechnologies.business_manager.modules.finance.payment.controller;

import com.IntegrityTechnologies.business_manager.config.OptimisticRetryRunner;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentDTO;
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

    /* ============================================================
       MAKE PAYMENT
       ============================================================ */
    @PostMapping
    public ResponseEntity<PaymentDTO> makePayment(@RequestBody PaymentRequest req) {
        return ResponseEntity.ok(
                OptimisticRetryRunner.runWithRetry(() -> paymentService.processPayment(req))
        );
    }

    /* ============================================================
       GET PAYMENT BY ID
       ============================================================ */
    @GetMapping("/{id}")
    public ResponseEntity<PaymentDTO> getPayment(@PathVariable UUID id) {
        return ResponseEntity.ok(paymentService.getPayment(id));
    }

    /* ============================================================
       LIST PAYMENTS
       ============================================================ */
    @GetMapping
    public ResponseEntity<Page<PaymentDTO>> listPayments(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size,
            @RequestParam(required = false) String method,
            @RequestParam(required = false) String status,
            @RequestParam(required = false) UUID saleId
    ) {
        return ResponseEntity.ok(
                paymentService.listPayments(page, size, method, status, saleId)
        );
    }

    /* ============================================================
       REFUND PAYMENT (SUCCESS only)
       ============================================================ */
    @PostMapping("/{id}/refund")
    public ResponseEntity<PaymentDTO> refundPayment(@PathVariable UUID id) {
        return ResponseEntity.ok(
                OptimisticRetryRunner.runWithRetry(() -> paymentService.refundPayment(id))
        );
    }

    /* ============================================================
       REVERSE PAYMENT (undo, accounting + customer ledger)
       ============================================================ */
    @PostMapping("/{id}/reverse")
    public ResponseEntity<PaymentDTO> reversePayment(
            @PathVariable UUID id,
            @RequestParam(required = false) String note
    ) {
        return ResponseEntity.ok(
                OptimisticRetryRunner.runWithRetry(() -> paymentService.reversePayment(id, note))
        );
    }

    /* ============================================================
       PAYMENT RECONCILIATION
       ============================================================ */
    @GetMapping("/reconcile")
    public ResponseEntity<Object> reconcilePayments(
            @RequestParam String from,
            @RequestParam String to
    ) {
        return ResponseEntity.ok(paymentService.reconcile(from, to));
    }
}