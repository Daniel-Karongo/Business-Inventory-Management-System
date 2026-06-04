package com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.base.mapper.TaxFilingMapper;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.dto.CorporateTaxAccrualPreviewDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.dto.CorporateTaxFilingDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.dto.CorporateTaxOverviewDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.model.CorporateTaxFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.repository.CorporateTaxFilingRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.service.CorporateTaxService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@RestController
@RequestMapping("/api/tax/corporate")
@RequiredArgsConstructor
@TenantManagerOnly
public class CorporateTaxController {

    private final CorporateTaxService corporateTaxService;
    private final CorporateTaxFilingRepository filingRepository;

    @GetMapping
    public Page<CorporateTaxFilingDTO> list(
            @RequestParam UUID branchId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "50") int size
    ) {

        return filingRepository
                .findByTenantIdAndBranchId(
                        TenantContext.getTenantId(),
                        branchId,
                        PageRequest.of(page,size)
                )
                .map(TaxFilingMapper::toDto);
    }

    @GetMapping("/overview")
    public CorporateTaxOverviewDTO overview(
            @RequestParam UUID branchId
    ) {
        return corporateTaxService.overview(
                branchId
        );
    }

    @GetMapping("/accrual-preview/{periodId}")
    public CorporateTaxAccrualPreviewDTO preview(
            @PathVariable UUID periodId,
            @RequestParam UUID branchId
    ) {
        return corporateTaxService.previewAccrual(
                periodId,
                branchId
        );
    }

    @PostMapping("/accrue/{periodId}")
    public CorporateTaxFiling accrue(
            @PathVariable UUID periodId,
            @RequestParam UUID branchId,
            Authentication authentication
    ) {
        return corporateTaxService.accrueCorporateTax(
                periodId,
                branchId,
                null,
                null,
                authentication.getName()
        );
    }

    @PostMapping("/pay/{filingId}")
    public void pay(
            @PathVariable UUID filingId,
            @RequestParam BigDecimal amount,
            @RequestParam UUID accountId,
            Authentication authentication
    ) {
        corporateTaxService.recordPayment(
                filingId,
                amount,
                accountId,
                authentication.getName()
        );
    }
}