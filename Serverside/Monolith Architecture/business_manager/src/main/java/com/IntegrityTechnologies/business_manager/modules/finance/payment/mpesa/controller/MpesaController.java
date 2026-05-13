package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model.MpesaTransaction;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.repository.MpesaTransactionRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.service.MpesaService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.util.Map;
import java.util.UUID;

@RestController
@RequestMapping("/api/payments/mpesa")
@RequiredArgsConstructor
@TenantUserOnly
public class MpesaController {

    private final MpesaService mpesaService;
    private final MpesaTransactionRepository repo;

    @PostMapping("/branch/{branchId}/stk/initiate")
    public ResponseEntity<MpesaTransaction> initStk(
            @PathVariable UUID branchId,
            @RequestParam UUID saleId,
            @RequestParam String phone,
            @RequestParam BigDecimal amount
    ) {

        return ResponseEntity.ok(
                mpesaService.initiateStk(
                        branchId,
                        saleId,
                        phone,
                        amount
                )
        );
    }

    @PostMapping("/branch/{branchId}/stk/callback")
    public ResponseEntity<String> stkCallback(
            @PathVariable UUID branchId,
            @RequestBody Map payload
    ) {

        mpesaService.handleStkCallback(
                branchId,
                payload
        );

        return ResponseEntity.ok(
                "{\"ResultCode\":0,\"ResultDesc\":\"Accepted\"}"
        );
    }

    @PostMapping("/branch/{branchId}/c2b/confirm")
    public ResponseEntity<String> c2bConfirm(
            @PathVariable UUID branchId,
            @RequestBody Map payload
    ) {

        mpesaService.handleC2BConfirm(
                branchId,
                payload
        );

        return ResponseEntity.ok(
                "{\"ResultCode\":0,\"ResultDesc\":\"Accepted\"}"
        );
    }

    @PostMapping("/branch/{branchId}/c2b/validate")
    public ResponseEntity<String> c2bValidate(
            @PathVariable UUID branchId,
            @RequestBody Map payload
    ) {

        mpesaService.handleC2BValidate(
                branchId,
                payload
        );

        return ResponseEntity.ok(
                "{\"ResultCode\":0,\"ResultDesc\":\"Accepted\"}"
        );
    }

    @GetMapping("/branch/{branchId}/{id}")
    public ResponseEntity<MpesaTransaction> get(
            @PathVariable UUID branchId,
            @PathVariable UUID id
    ) {

        return repo.findByTenantIdAndBranchIdAndId(
                        TenantContext.getTenantId(),
                        branchId,
                        id
                )
                .map(ResponseEntity::ok)
                .orElse(ResponseEntity.notFound().build());
    }
}