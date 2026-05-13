package com.IntegrityTechnologies.business_manager.modules.finance.payment.base.controller;

import com.IntegrityTechnologies.business_manager.config.spring.OptimisticRetryRunner;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.dto.PaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.service.PaymentService;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.dto.PaymentDTO;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
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
@TenantUserOnly
public class PaymentController {

    private final PaymentService paymentService;

    @PostMapping("/branch/{branchId}")
    public ResponseEntity<PaymentDTO> makePayment(
            @PathVariable UUID branchId,
            @RequestBody PaymentRequest req
    ) {

        return ResponseEntity.ok(
                OptimisticRetryRunner.runWithRetry(
                        () -> paymentService.processPayment(
                                branchId,
                                req
                        )
                )
        );
    }

    @GetMapping("/branch/{branchId}/{id}")
    public ResponseEntity<PaymentDTO> getPayment(
            @PathVariable UUID branchId,
            @PathVariable UUID id
    ) {

        return ResponseEntity.ok(
                paymentService.getPayment(
                        branchId,
                        id
                )
        );
    }

    @GetMapping("/branch/{branchId}")
    public ResponseEntity<Page<PaymentDTO>> listPayments(
            @PathVariable UUID branchId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size,
            @RequestParam(required = false) String method,
            @RequestParam(required = false) String status,
            @RequestParam(required = false) UUID saleId
    ) {

        return ResponseEntity.ok(
                paymentService.listPayments(
                        branchId,
                        page,
                        size,
                        method,
                        status,
                        saleId
                )
        );
    }

    @PostMapping("/branch/{branchId}/{id}/refund")
    public ResponseEntity<PaymentDTO> refundPayment(
            @PathVariable UUID branchId,
            @PathVariable UUID id
    ) {

        return ResponseEntity.ok(
                OptimisticRetryRunner.runWithRetry(
                        () -> paymentService.refundPayment(
                                branchId,
                                id
                        )
                )
        );
    }

    @PostMapping("/branch/{branchId}/{id}/reverse")
    public ResponseEntity<PaymentDTO> reversePayment(
            @PathVariable UUID branchId,
            @PathVariable UUID id,
            @RequestParam(required = false) String note
    ) {

        return ResponseEntity.ok(
                OptimisticRetryRunner.runWithRetry(
                        () -> paymentService.reversePayment(
                                branchId,
                                id,
                                note
                        )
                )
        );
    }

    @GetMapping("/branch/{branchId}/reconcile")
    public ResponseEntity<Object> reconcilePayments(
            @PathVariable UUID branchId,
            @RequestParam String from,
            @RequestParam String to
    ) {

        return ResponseEntity.ok(
                paymentService.reconcile(
                        branchId,
                        from,
                        to
                )
        );
    }
}