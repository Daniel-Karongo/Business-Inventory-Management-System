package com.IntegrityTechnologies.business_manager.modules.procurement.matching.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.procurement.matching.dto.ReceiveAndInvoiceRequest;
import com.IntegrityTechnologies.business_manager.modules.procurement.matching.dto.ReceiveAndInvoiceResult;
import com.IntegrityTechnologies.business_manager.modules.procurement.matching.service.ReceiveAndInvoiceService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/procurement/receive-and-invoice")
@RequiredArgsConstructor
@TenantManagerOnly
public class ReceiveAndInvoiceController {

    private final ReceiveAndInvoiceService
            service;

    @PostMapping
    public ReceiveAndInvoiceResult execute(
            @Valid
            @RequestBody
            ReceiveAndInvoiceRequest request
    ) {

        return service.execute(request);
    }
}