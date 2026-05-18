package com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.domain.SupplierPaymentAllocation;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.dto.AllocateSupplierPaymentRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.service.ApAllocationService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/finance/ap/allocations")
@RequiredArgsConstructor
@TenantManagerOnly
public class ApAllocationController {

    private final ApAllocationService
            service;

    @PostMapping
    public SupplierPaymentAllocation allocate(
            @Valid
            @RequestBody
            AllocateSupplierPaymentRequest request
    ) {

        return service.allocate(request);
    }
}