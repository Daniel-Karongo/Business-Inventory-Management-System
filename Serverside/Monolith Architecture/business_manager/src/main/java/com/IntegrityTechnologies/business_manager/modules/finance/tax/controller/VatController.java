package com.IntegrityTechnologies.business_manager.modules.finance.tax.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.TaxPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.VatFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.dto.VatFilingDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.mapper.TaxFilingMapper;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.VatFilingRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.service.VatFilingService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.IntegrityTechnologies.business_manager.security.BranchTenantGuard;

import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.*;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/tax/vat")
@RequiredArgsConstructor
@TenantManagerOnly
public class VatController {

    private final VatFilingService filingService;
    private final VatFilingRepository filingRepo;
    private final BranchTenantGuard branchTenantGuard;

    @PostMapping("/file/{periodId}")
    public VatFilingDTO file(
            @PathVariable UUID periodId,
            @RequestParam UUID branchId
    ) {

        branchTenantGuard.validate(branchId);

        TaxPeriod period = new TaxPeriod();
        period.setId(periodId);

        return TaxFilingMapper.toDto(
                filingService.file(period, branchId, currentUser())
        );
    }

    @PostMapping("/pay/{filingId}")
    public void pay(
            @PathVariable UUID filingId,
            @RequestParam UUID accountId
    ) {

        VatFiling filing =
                filingRepo.findByTenantIdAndId(
                        TenantContext.getTenantId(),
                        filingId
                ).orElseThrow(() ->
                        new IllegalArgumentException("Filing not found"));

        filingService.markPaid(filing, currentUser(), accountId);
    }

    @GetMapping
    public Page<VatFiling> list(
            @RequestParam UUID branchId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size
    ) {

        branchTenantGuard.validate(branchId);

        Pageable pageable = PageRequest.of(page, size);

        return filingRepo.findByTenantIdAndBranchId(
                TenantContext.getTenantId(),
                branchId,
                pageable
        );
    }

    private String currentUser() {

        var auth = SecurityContextHolder.getContext().getAuthentication();

        return auth != null ? auth.getName() : "SYSTEM";
    }
}