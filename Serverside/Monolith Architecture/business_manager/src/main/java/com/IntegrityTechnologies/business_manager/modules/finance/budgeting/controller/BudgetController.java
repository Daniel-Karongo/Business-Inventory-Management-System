package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.controller;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.dto.BranchComparisonDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.dto.CorporateVarianceDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.service.BudgetService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.security.BranchTenantGuard;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.*;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/budgets")
@RequiredArgsConstructor
@TenantManagerOnly
public class BudgetController {

    private final BudgetService budgetService;
    private final BranchTenantGuard branchTenantGuard;

    @PostMapping
    public ApiResponse create(
            @RequestParam UUID branchId,
            @RequestParam int fiscalYear
    ) {

        branchTenantGuard.validate(branchId);

        return new ApiResponse(
                "success",
                "Budget created",
                budgetService.createBudget(
                        branchId,
                        fiscalYear,
                        "SYSTEM"
                )
        );
    }

    @PostMapping("/{budgetId}/lines")
    public ApiResponse upsertLine(
            @PathVariable UUID budgetId,
            @RequestParam UUID accountId,
            @RequestParam int month,
            @RequestParam double amount
    ) {

        budgetService.upsertBudgetMonth(
                budgetId,
                accountId,
                month,
                java.math.BigDecimal.valueOf(amount)
        );

        return new ApiResponse("success", "Budget month updated", null);
    }

    @GetMapping("/variance")
    public CorporateVarianceDTO branchVariance(
            @RequestParam UUID branchId,
            @RequestParam int year,
            @RequestParam int month,
            @RequestParam UUID accountId
    ) {

        branchTenantGuard.validate(branchId);

        return budgetService.computeBranchVariance(
                branchId,
                year,
                month,
                accountId
        );
    }

    @GetMapping("/consolidated")
    public CorporateVarianceDTO consolidated(
            @RequestParam int year,
            @RequestParam int month,
            @RequestParam UUID accountId
    ) {

        return budgetService.computeCorporateVariance(
                year,
                month,
                accountId
        );
    }

    @GetMapping("/compare")
    public Page<BranchComparisonDTO> compareBranches(
            @RequestParam int year,
            @RequestParam int month,
            @RequestParam UUID accountId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size
    ) {

        List<BranchComparisonDTO> full =
                budgetService.compareBranches(year, month, accountId);

        int start = Math.min(page * size, full.size());
        int end = Math.min(start + size, full.size());

        return new PageImpl<>(
                full.subList(start, end),
                PageRequest.of(page, size),
                full.size()
        );
    }
}