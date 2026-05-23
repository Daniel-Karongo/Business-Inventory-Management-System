package com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.controller;

import com.IntegrityTechnologies.business_manager.config.response.PageWrapper;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.dto.*;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.enums.SupplierPaymentStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.service.SupplierPaymentPostingService;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.service.SupplierPaymentProcessingService;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.service.SupplierPaymentService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/finance/supplier-payments")
@RequiredArgsConstructor
@TenantManagerOnly
public class SupplierPaymentController {

    private final SupplierPaymentService service;

    private final SupplierPaymentPostingService postingService;
    private final SupplierPaymentProcessingService processingService;

    @PostMapping
    public SupplierPaymentResponse create(
            @Valid
            @RequestBody
            CreateSupplierPaymentRequest request
    ) {
        return service.create(request);
    }

    @PostMapping("/{paymentId}/post")
    public SupplierPaymentResponse post(
            @RequestParam UUID branchId,
            @PathVariable UUID paymentId
    ) {
        return postingService.post(
                branchId,
                paymentId
        );
    }

    @PostMapping("/process")
    public SupplierPaymentResponse process(
            @Valid
            @RequestBody
            ProcessSupplierPaymentRequest request
    ) {
        return processingService.process(request);
    }

    @PostMapping("/{paymentId}/reverse")
    public SupplierPaymentResponse reverse(
            @RequestParam UUID branchId,
            @PathVariable UUID paymentId,
            @Valid @RequestBody ReverseSupplierPaymentRequest request
    ) {
        return postingService.reverse(
                branchId,
                paymentId,
                request.getReason()
        );
    }

    @GetMapping("/{paymentId}")
    public SupplierPaymentDetailsResponse get(
            @RequestParam UUID branchId,
            @PathVariable UUID paymentId
    ) {
        return service.details(
                branchId,
                paymentId
        );
    }

    @GetMapping
    public PageWrapper<SupplierPaymentResponse> list(
            @RequestParam UUID branchId,
            @RequestParam(required = false)
            UUID supplierId,
            @RequestParam(required = false)
            SupplierPaymentStatus status,
            @RequestParam(defaultValue = "0")
            int page,
            @RequestParam(defaultValue = "20")
            int size
    ) {
        return new PageWrapper<>(
                service.list(
                        branchId,
                        supplierId,
                        status,
                        PageRequest.of(
                                page,
                                size,
                                Sort.by(
                                        Sort.Direction.DESC,
                                        "paymentDate"
                                )
                        )
                )
        );
    }

    @PostMapping("/search")
    public PageWrapper<SupplierPaymentResponse> search(
            @RequestParam UUID branchId,
            @RequestBody SupplierPaymentSearchRequest request,
            @RequestParam(defaultValue = "0")
            int page,
            @RequestParam(defaultValue = "20")
            int size
    ) {
        return new PageWrapper<>(
                service.search(
                        branchId,
                        request,
                        PageRequest.of(
                                page,
                                size,
                                Sort.by(
                                        Sort.Direction.DESC,
                                        "paymentDate"
                                )
                        )
                )
        );
    }

    @GetMapping("/funding-accounts")
    public List<FundingAccountResponse> fundingAccounts(
            @RequestParam UUID branchId
    ) {
        return service.getFundingAccounts(
                branchId
        );
    }
}