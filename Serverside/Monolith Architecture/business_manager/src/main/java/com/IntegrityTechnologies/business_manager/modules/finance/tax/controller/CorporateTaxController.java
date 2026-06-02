package com.IntegrityTechnologies.business_manager.modules.finance.tax.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.CorporateTaxFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.TaxPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.CorporateTaxFilingRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.TaxPeriodRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.service.CorporateTaxService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;

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
    private final TaxPeriodRepository taxPeriodRepository;

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
            @RequestParam UUID branchId
    ) {

        branchTenantGuard.validate(branchId);

        TaxPeriod period =
                taxPeriodRepository
                        .findByTenantIdAndId(
                                TenantContext.getTenantId(),
                                periodId
                        )
                        .orElseThrow();

        return service.accrueCorporateTax(
                periodId,
                branchId,
                period.getStartDate().atStartOfDay(),
                period.getEndDate().plusDays(1).atStartOfDay(),
                SecurityUtils.currentUsername()
        );
    }

    @PostMapping("/pay/{filingId}")
    public void pay(
            @PathVariable UUID filingId,
            @RequestParam UUID accountId,
            @RequestParam UUID branchId
    ) {

        branchTenantGuard.validate(branchId);

        service.markPaid(
                filingId,
                SecurityUtils.currentUsername(),
                accountId
        );
    }
}