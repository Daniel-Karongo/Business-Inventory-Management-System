package com.IntegrityTechnologies.business_manager.modules.finance.payment.base.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.domain.SupplierPayment;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.service.SupplierPaymentService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.util.UUID;

@RestController
@RequestMapping("/api/finance/supplier-payments")
@RequiredArgsConstructor
@TenantManagerOnly
public class SupplierPaymentController {

    private final SupplierPaymentService service;

    @PostMapping
    public SupplierPayment pay(
            @RequestParam UUID branchId,
            @RequestParam UUID supplierId,
            @RequestParam BigDecimal amount,
            @RequestParam String method,
            @RequestParam(required = false) String reference
    ) {

        return service.pay(
                branchId,
                supplierId,
                amount,
                method,
                reference
        );
    }

    @GetMapping
    public Page<SupplierPayment> list(
            @RequestParam UUID branchId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size
    ) {

        return service.list(
                branchId,
                PageRequest.of(page, size)
        );
    }
}