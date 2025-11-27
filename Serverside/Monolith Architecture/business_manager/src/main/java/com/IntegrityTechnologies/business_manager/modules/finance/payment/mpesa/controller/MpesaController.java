package com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.model.MpesaTransaction;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.repository.MpesaTransactionRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.mpesa.service.MpesaService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.util.Map;
import java.util.UUID;

@RestController
@RequestMapping("/api/payments/mpesa")
@RequiredArgsConstructor
public class MpesaController {

    private final MpesaService mpesaService;
    private final MpesaTransactionRepository repo;

    @PostMapping("/stk/initiate")
    public ResponseEntity<MpesaTransaction> initStk(@RequestParam UUID saleId,
                                                    @RequestParam String phone,
                                                    @RequestParam BigDecimal amount) {
        MpesaTransaction tx = mpesaService.initiateStk(saleId, phone, amount);
        return ResponseEntity.ok(tx);
    }

    @PostMapping("/stk/callback")
    public ResponseEntity<String> stkCallback(@RequestBody Map payload) {
        // called by Safaricom — process and return 200
        mpesaService.handleStkCallback(payload);
        return ResponseEntity.ok("{\"ResultCode\":0,\"ResultDesc\":\"Accepted\"}");
    }

    // C2B endpoints for confirm & validate (Safaricom will POST here)
    @PostMapping("/c2b/confirm")
    public ResponseEntity<String> c2bConfirm(@RequestBody Map payload) {
        // implement logic: create payment, mark sale paid etc.
        // For now just log and respond with success body expected by Daraja
        // e.g., {"ResultCode":0,"ResultDesc":"Accepted"}
        System.out.println("C2B confirm: " + payload);
        return ResponseEntity.ok("{\"ResultCode\":0,\"ResultDesc\":\"Accepted\"}");
    }

    @PostMapping("/c2b/validate")
    public ResponseEntity<String> c2bValidate(@RequestBody Map payload) {
        System.out.println("C2B validate: " + payload);
        return ResponseEntity.ok("{\"ResultCode\":0,\"ResultDesc\":\"Accepted\"}");
    }

    @GetMapping("/{id}")
    public ResponseEntity<MpesaTransaction> get(@PathVariable UUID id) {
        return repo.findById(id).map(ResponseEntity::ok).orElseGet(() -> ResponseEntity.notFound().build());
    }
}