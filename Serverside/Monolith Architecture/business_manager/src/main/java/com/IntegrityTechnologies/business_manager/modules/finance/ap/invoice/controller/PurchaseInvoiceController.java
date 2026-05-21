package com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.dto.CreatePurchaseInvoiceRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.dto.PurchaseInvoiceResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.service.PurchaseInvoicePostingService;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.service.PurchaseInvoiceReversalService;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.service.PurchaseInvoiceService;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.shared.mapper.PurchaseInvoiceMapper;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/finance/ap/invoices")
@RequiredArgsConstructor
@TenantManagerOnly
public class PurchaseInvoiceController {

    private final PurchaseInvoiceService service;
    private final PurchaseInvoiceMapper mapper;
    private final PurchaseInvoicePostingService postingService;
    private final PurchaseInvoiceReversalService reversalService;

    @PostMapping
    public PurchaseInvoiceResponse create(
            @Valid
            @RequestBody
            CreatePurchaseInvoiceRequest request
    ) {

        return service.create(request);
    }

    @PostMapping("/{invoiceId}/approve")
    public PurchaseInvoiceResponse approve(
            @RequestParam UUID branchId,
            @PathVariable UUID invoiceId
    ) {

        return service.markApproved(
                branchId,
                invoiceId
        );
    }

    @PostMapping("/{invoiceId}/cancel")
    public PurchaseInvoiceResponse cancel(
            @RequestParam UUID branchId,
            @PathVariable UUID invoiceId,
            @RequestParam String reason
    ) {

        return service.markCancelled(
                branchId,
                invoiceId,
                reason
        );
    }

    @GetMapping("/{invoiceId}")
    public PurchaseInvoiceResponse get(
            @RequestParam UUID branchId,
            @PathVariable UUID invoiceId
    ) {

        return service.get(
                branchId,
                invoiceId
        );
    }

    @PostMapping("/{invoiceId}/post")
    public PurchaseInvoiceResponse post(
            @RequestParam UUID branchId,
            @PathVariable UUID invoiceId
    ) {

        return mapper.toResponse(
                postingService.post(
                        branchId,
                        invoiceId
                )
        );
    }

    @PostMapping("/{invoiceId}/reverse")
    public PurchaseInvoiceResponse reverse(
            @RequestParam UUID branchId,
            @PathVariable UUID invoiceId,
            @RequestParam String reason
    ) {

        return mapper.toResponse(
                reversalService.reverse(
                        branchId,
                        invoiceId,
                        reason,
                        "SYSTEM"
                )
        );
    }
}