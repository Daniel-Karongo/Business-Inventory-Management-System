package com.IntegrityTechnologies.business_manager.modules.finance.sales.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.SaleRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.dto.SaleResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.service.SalesService;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Sales")
@RestController
@RequestMapping("/api/sales")
@RequiredArgsConstructor
public class SalesController {

    private final SalesService salesService;

    @PreAuthorize("hasAnyRole('ADMIN','CASHIER','MANAGER')")
    @PostMapping
    public ResponseEntity<SaleResponse> createSale(@RequestBody SaleRequest req) {
        return ResponseEntity.ok(salesService.createSale(req));
    }

    // TODO: GET endpoints for sale by id, list, and reports (next steps)
}