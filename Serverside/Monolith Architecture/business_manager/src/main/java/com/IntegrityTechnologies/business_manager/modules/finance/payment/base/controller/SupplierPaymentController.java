package com.IntegrityTechnologies.business_manager.modules.finance.payment.base.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.dto.CreateSupplierPaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.dto.SupplierPaymentResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.payment.base.service.SupplierPaymentService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/finance/supplier-payments")
@RequiredArgsConstructor
@TenantManagerOnly
public class SupplierPaymentController {

    private final SupplierPaymentService
            service;

    @PostMapping
    public SupplierPaymentResponse create(
            @Valid
            @RequestBody
            CreateSupplierPaymentRequest request
    ) {

        return service.create(request);
    }

    @GetMapping
    public Page<SupplierPaymentResponse> list(
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