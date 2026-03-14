package com.IntegrityTechnologies.business_manager.modules.finance.tax.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.CorporateTaxFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.CorporateTaxFilingRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.service.CorporateTaxService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.IntegrityTechnologies.business_manager.security.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;

import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.*;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.UUID;

@RestController
@RequestMapping("/api/tax/corporate")
@RequiredArgsConstructor
@TenantManagerOnly
public class CorporateTaxController {

    private final CorporateTaxService service;
    private final CorporateTaxFilingRepository repository;
    private final BranchTenantGuard branchTenantGuard;

    @GetMapping
    public Page<CorporateTaxFiling> list(
            @RequestParam UUID branchId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size
    ) {

        branchTenantGuard.validate(branchId);

        Pageable pageable = PageRequest.of(page, size);

        return repository.findByTenantIdAndBranchId(
                TenantContext.getTenantId(),
                branchId,
                pageable
        );
    }

    @PostMapping("/accrue/{periodId}")
    public CorporateTaxFiling accrue(
            @PathVariable UUID periodId,
            @RequestParam UUID branchId,
            @RequestParam String from,
            @RequestParam String to
    ) {

        branchTenantGuard.validate(branchId);

        return service.accrueCorporateTax(
                periodId,
                branchId,
                LocalDateTime.parse(from),
                LocalDateTime.parse(to),
                SecurityUtils.currentUsername()
        );
    }

    @PostMapping("/pay/{filingId}")
    public void pay(
            @PathVariable UUID filingId,
            @RequestParam UUID accountId
    ) {

        service.markPaid(
                filingId,
                SecurityUtils.currentUsername(),
                accountId
        );
    }
}