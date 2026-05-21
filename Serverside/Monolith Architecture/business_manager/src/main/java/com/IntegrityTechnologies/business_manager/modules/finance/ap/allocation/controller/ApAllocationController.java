package com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.dto.*;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.service.ApAllocationService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/finance/ap/allocations")
@RequiredArgsConstructor
@TenantManagerOnly
public class ApAllocationController {

    private final ApAllocationService service;

    @PostMapping("/manual")
    public AllocationResponse manualAllocate(
            @Valid
            @RequestBody
            AllocateSupplierPaymentRequest request
    ) {
        return service.allocate(
                request
        );
    }

    @PostMapping("/preview")
    public AllocationPreviewResponse preview(
            @Valid
            @RequestBody
            AutoAllocatePaymentRequest request
    ) {
        return service.previewAutoAllocation(
                request
        );
    }

    @PostMapping("/auto")
    public List<AllocationResponse> autoAllocate(
            @Valid
            @RequestBody
            AutoAllocatePaymentRequest request
    ) {
        return service.autoAllocate(
                request
        );
    }

    @PostMapping("/{allocationId}/reverse")
    public AllocationResponse reverse(
            @RequestParam UUID branchId,
            @PathVariable UUID allocationId,
            @Valid
            @RequestBody
            ReverseAllocationRequest request
    ) {
        return service.reverse(
                branchId,
                allocationId,
                request.getReason()
        );
    }
}