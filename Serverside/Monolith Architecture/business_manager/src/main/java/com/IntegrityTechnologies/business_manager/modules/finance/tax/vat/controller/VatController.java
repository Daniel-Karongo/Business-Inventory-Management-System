package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountingPeriodRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.VatFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto.VatFilingDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.base.mapper.TaxFilingMapper;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository.VatFilingRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.service.VatFilingService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.util.UUID;

@RestController
@RequestMapping("/api/tax/vat")
@RequiredArgsConstructor
@TenantManagerOnly
public class VatController {

    private final VatFilingService filingService;
    private final VatFilingRepository filingRepo;
    private final BranchTenantGuard branchTenantGuard;
    private final AccountingPeriodRepository periodRepository;

    @PostMapping("/file/{periodId}")
    public VatFilingDTO file(
            @PathVariable UUID periodId,
            @RequestParam UUID branchId
    ) {
        branchTenantGuard.validate(branchId);

        AccountingPeriod period =
                periodRepository
                        .findByTenantIdAndBranchIdAndId(
                                TenantContext.getTenantId(),
                                branchId,
                                periodId
                        )
                        .orElseThrow(() ->
                                new IllegalArgumentException(
                                        "Accounting period not found"
                                ));

        return TaxFilingMapper.toDto(
                filingService.file(
                        period,
                        branchId,
                        currentUser()
                )
        );
    }

    @PostMapping("/pay/{filingId}")
    public void pay(
            @PathVariable UUID filingId,
            @RequestParam UUID accountId,
            @RequestParam UUID branchId
    ) {

        branchTenantGuard.validate(branchId);

        VatFiling filing =
                filingRepo.findByTenantIdAndId(
                        TenantContext.getTenantId(),
                        filingId
                ).orElseThrow(() ->
                        new IllegalArgumentException(
                                "Filing not found"
                        ));

        filingService.markPaid(
                filing,
                currentUser(),
                accountId
        );
    }

    @PostMapping("/refund/request/{filingId}")
    public void requestRefund(
            @PathVariable UUID filingId
    ) {
        filingService.requestRefund(
                filingId,
                currentUser()
        );
    }

    @PostMapping("/refund/complete/{filingId}")
    public void completeRefund(
            @PathVariable UUID filingId,
            @RequestParam UUID accountId
    ) {
        filingService.completeRefund(
                filingId,
                accountId,
                currentUser()
        );
    }

    @GetMapping
    public Page<VatFilingDTO> list(
            @RequestParam UUID branchId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size
    ) {
        branchTenantGuard.validate(branchId);

        Pageable pageable = PageRequest.of(page, size);

        return filingRepo
                .findByTenantIdAndBranchIdOrderByFiledAtDesc(
                        TenantContext.getTenantId(),
                        branchId,
                        pageable
                )
                .map(TaxFilingMapper::toDto);
    }

    private String currentUser() {

        var auth = SecurityContextHolder.getContext().getAuthentication();

        return auth != null ? auth.getName() : "SYSTEM";
    }
}