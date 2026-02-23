package com.IntegrityTechnologies.business_manager.modules.finance.payment.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.domain.SupplierPayment;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.service.SupplierPaymentService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.util.UUID;

@RestController
@RequestMapping("/api/finance/supplier-payments")
@RequiredArgsConstructor
public class SupplierPaymentController {

    private final SupplierPaymentService service;

    @PostMapping
    public SupplierPayment pay(
            @RequestParam UUID supplierId,
            @RequestParam BigDecimal amount,
            @RequestParam String method,
            @RequestParam(required = false) String reference
    ) {
        return service.pay(supplierId, amount, method, reference);
    }

    @GetMapping
    public Page<SupplierPayment> list(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size
    ) {
        return service.list(PageRequest.of(page, size));
    }
}