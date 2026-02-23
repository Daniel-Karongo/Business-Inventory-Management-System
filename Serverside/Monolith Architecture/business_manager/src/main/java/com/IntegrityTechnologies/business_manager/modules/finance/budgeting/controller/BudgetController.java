package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.controller;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.dto.BranchComparisonDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.dto.CorporateVarianceDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.service.BudgetService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/budgets")
@RequiredArgsConstructor
public class BudgetController {

    private final BudgetService budgetService;

    /* ============================================================
       CREATE BUDGET
       ============================================================ */

    @PostMapping
    public ApiResponse create(
            @RequestParam(required = false) UUID branchId,
            @RequestParam int fiscalYear
    ) {

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

    /* ============================================================
       UPSERT MONTH VALUE
       ============================================================ */

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

    /* ============================================================
       BRANCH VARIANCE
       ============================================================ */

    @GetMapping("/variance")
    public CorporateVarianceDTO branchVariance(
            @RequestParam UUID branchId,
            @RequestParam int year,
            @RequestParam int month,
            @RequestParam UUID accountId
    ) {

        return budgetService.computeBranchVariance(
                branchId,
                year,
                month,
                accountId
        );
    }

    /* ============================================================
       CORPORATE CONSOLIDATED VARIANCE
       ============================================================ */

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
    public List<BranchComparisonDTO> compareBranches(
            @RequestParam int year,
            @RequestParam int month,
            @RequestParam UUID accountId
    ) {

        return budgetService.compareBranches(
                year,
                month,
                accountId
        );
    }
}