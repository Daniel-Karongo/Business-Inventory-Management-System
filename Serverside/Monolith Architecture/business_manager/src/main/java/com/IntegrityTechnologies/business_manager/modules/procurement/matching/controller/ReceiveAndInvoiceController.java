package com.IntegrityTechnologies.business_manager.modules.procurement.matching.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.dto.PurchaseInvoiceResponse;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.procurement.matching.dto.ReceiveAndInvoiceRequest;
import com.IntegrityTechnologies.business_manager.modules.procurement.matching.service.ReceiveAndInvoiceService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/procurement/receive-and-invoice")
@RequiredArgsConstructor
@TenantManagerOnly
public class ReceiveAndInvoiceController {

    private final ReceiveAndInvoiceService
            service;

    @PostMapping
    public PurchaseInvoiceResponse execute(
            @Valid
            @RequestBody
            ReceiveAndInvoiceRequest request
    ) {

        return service.execute(request);
    }
}