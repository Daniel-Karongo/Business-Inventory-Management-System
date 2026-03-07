package com.IntegrityTechnologies.business_manager.modules.finance.tax.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.CorporateTaxFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.CorporateTaxFilingRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.service.CorporateTaxService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
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

    @GetMapping
    public Page<CorporateTaxFiling> list(
            @RequestParam(required = false) UUID branchId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size
    ) {

        Pageable pageable = PageRequest.of(page, size);

        if (branchId != null) {
            return repository.findByBranchId(branchId, pageable);
        }

        return repository.findAll(pageable);
    }

    @PostMapping("/accrue/{periodId}")
    public CorporateTaxFiling accrue(
            @PathVariable UUID periodId,
            @RequestParam UUID branchId,
            @RequestParam String from,
            @RequestParam String to
    ) {
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