package com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.SellableResolveRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.dto.SellableResolveResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.sellable.service.SellableApplicationService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/sellable")
@RequiredArgsConstructor
public class SellableController {

    private final SellableApplicationService service;

    @PostMapping("/resolve")
    public SellableResolveResponse resolve(
            @RequestBody SellableResolveRequest request
    ) {
        return service.resolve(request);
    }
}