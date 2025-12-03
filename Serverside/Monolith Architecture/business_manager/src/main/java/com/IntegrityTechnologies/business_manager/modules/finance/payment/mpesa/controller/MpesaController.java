package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model.MpesaTransaction;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.repository.MpesaTransactionRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.service.MpesaService;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.util.Map;
import java.util.UUID;

@Tag(name = "Mpesa")
@RestController
@RequestMapping("/api/payments/mpesa")
@RequiredArgsConstructor
public class MpesaController {

    private final MpesaService mpesaService;
    private final MpesaTransactionRepository repo;

    /** 1. Initiate STK Push */
    @PostMapping("/stk/initiate")
    public ResponseEntity<MpesaTransaction> initStk(@RequestParam UUID saleId,
                                                    @RequestParam String phone,
                                                    @RequestParam BigDecimal amount) {
        return ResponseEntity.ok(
                mpesaService.initiateStk(saleId, phone, amount)
        );
    }

    /** 2. Handle STK Callback */
    @PostMapping("/stk/callback")
    public ResponseEntity<String> stkCallback(@RequestBody Map payload) {
        mpesaService.handleStkCallback(payload);
        return ResponseEntity.ok("{\"ResultCode\":0,\"ResultDesc\":\"Accepted\"}");
    }

    /** 3. C2B Confirm */
    @PostMapping("/c2b/confirm")
    public ResponseEntity<String> c2bConfirm(@RequestBody Map payload) {
        mpesaService.handleC2BConfirm(payload); // <-- optionally implement
        return ResponseEntity.ok("{\"ResultCode\":0,\"ResultDesc\":\"Accepted\"}");
    }

    /** 4. C2B Validate */
    @PostMapping("/c2b/validate")
    public ResponseEntity<String> c2bValidate(@RequestBody Map payload) {
        mpesaService.handleC2BValidate(payload); // <-- optionally implement
        return ResponseEntity.ok("{\"ResultCode\":0,\"ResultDesc\":\"Accepted\"}");
    }

    /** 5. Get Transaction */
    @GetMapping("/{id}")
    public ResponseEntity<MpesaTransaction> get(@PathVariable UUID id) {
        return repo.findById(id)
                .map(ResponseEntity::ok)
                .orElse(ResponseEntity.notFound().build());
    }
}