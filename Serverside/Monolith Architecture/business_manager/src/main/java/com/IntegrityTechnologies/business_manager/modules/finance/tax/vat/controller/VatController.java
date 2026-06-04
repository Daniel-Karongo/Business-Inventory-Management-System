package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountingPeriodRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.base.mapper.TaxFilingMapper;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto.*;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository.VatFilingRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.service.*;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.util.List;
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
    private final VatQueryService vatQueryService;
    private final VatPreviewService vatPreviewService;
    private final VatReadinessService vatReadinessService;
    private final VatHistoryService vatHistoryService;
    private final VatPaymentQueryService vatPaymentQueryService;
    private final VatRefundQueryService vatRefundQueryService;
    private final VatCreditMovementQueryService vatCreditMovementQueryService;

    @GetMapping("/overview")
    public VatOverviewResponse overview(
            @RequestParam UUID branchId
    ) {

        branchTenantGuard.validate(
                branchId
        );

        return vatQueryService.overview(
                branchId
        );
    }

    @GetMapping("/refunds")
    public List<VatRefundResponse> refunds(
            @RequestParam UUID branchId
    ) {

        branchTenantGuard.validate(
                branchId
        );

        return vatRefundQueryService.list(
                branchId
        );
    }

    @GetMapping("/credit-history")
    public List<VatCreditMovementResponse> creditHistory(
            @RequestParam UUID branchId
    ) {

        branchTenantGuard.validate(
                branchId
        );

        return vatCreditMovementQueryService.list(
                branchId
        );
    }

    @GetMapping("/current-credit")
    public BigDecimal currentCredit(
            @RequestParam UUID branchId
    ) {
        branchTenantGuard.validate(
                branchId
        );

        return vatQueryService.currentCredit(
                branchId
        );
    }

    @GetMapping("/history")
    public List<VatFilingSummaryResponse> history(
            @RequestParam UUID branchId
    ) {

        branchTenantGuard.validate(
                branchId
        );

        return vatHistoryService.history(
                branchId
        );
    }

    @GetMapping("/detail/{filingId}")
    public VatFilingDetailResponse detail(
            @PathVariable UUID filingId
    ) {

        return vatHistoryService.detail(
                filingId
        );
    }

    @GetMapping("/payments/{filingId}")
    public List<VatPaymentResponse> payments(
            @PathVariable UUID filingId
    ) {

        return vatPaymentQueryService.list(
                filingId
        );
    }

    @GetMapping("/dashboard")
    public VatDashboardResponse dashboard(
            @RequestParam UUID branchId
    ) {

        branchTenantGuard.validate(
                branchId
        );

        return vatQueryService.dashboard(
                branchId
        );
    }

    @GetMapping("/preview/{periodId}")
    public VatFilingPreviewResponse preview(
            @PathVariable UUID periodId,
            @RequestParam UUID branchId
    ) {

        branchTenantGuard.validate(
                branchId
        );

        AccountingPeriod period =
                periodRepository
                        .findByTenantIdAndBranchIdAndId(
                                TenantContext.getTenantId(),
                                branchId,
                                periodId
                        )
                        .orElseThrow(
                                () ->
                                        new IllegalArgumentException(
                                                "Accounting period not found"
                                        )
                        );

        return vatPreviewService.preview(
                period,
                branchId
        );
    }

    @GetMapping("/readiness/{periodId}")
    public VatFilingReadinessResponse readiness(
            @PathVariable UUID periodId,
            @RequestParam UUID branchId
    ) {

        branchTenantGuard.validate(
                branchId
        );

        AccountingPeriod period =
                periodRepository
                        .findByTenantIdAndBranchIdAndId(
                                TenantContext.getTenantId(),
                                branchId,
                                periodId
                        )
                        .orElseThrow(
                                () ->
                                        new IllegalArgumentException(
                                                "Accounting period not found"
                                        )
                        );

        return vatReadinessService.check(
                period,
                branchId
        );
    }

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

    @PostMapping("/payment/{filingId}")
    public void recordPayment(
            @PathVariable UUID filingId,
            @Valid
            @RequestBody RecordVatPaymentRequest request
    ) {

        filingService.recordPayment(
                filingId,
                request.amount(),
                request.fundingAccountId(),
                currentUser()
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