package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.controller;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain.Budget;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.domain.enums.BudgetScenario;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.repository.BudgetRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.service.BudgetService;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantSupervisorOnly;
import com.IntegrityTechnologies.business_manager.security.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.*;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/budget-management")
@RequiredArgsConstructor
@TenantSupervisorOnly
public class BudgetManagementController {

    private final BudgetRepository budgetRepository;
    private final BudgetService budgetService;
    private final BranchTenantGuard branchTenantGuard;

    @GetMapping
    public Page<Budget> list(
            @RequestParam UUID branchId,
            @RequestParam int fiscalYear,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size
    ) {

        branchTenantGuard.validate(branchId);

        Pageable pageable = PageRequest.of(page, size);

        return budgetRepository
                .findByTenantIdAndBranchIdAndFiscalYearAndScenario(
                        TenantContext.getTenantId(),
                        branchId,
                        fiscalYear,
                        BudgetScenario.BASELINE,
                        pageable
                );
    }

    @GetMapping("/{id}")
    public Budget get(@PathVariable UUID id) {

        return budgetRepository.findByTenantIdAndId(
                TenantContext.getTenantId(),
                id
        ).orElseThrow(() -> new IllegalArgumentException("Budget not found"));
    }

    @PostMapping("/{id}/submit")
    public ApiResponse submit(@PathVariable UUID id) {

        budgetService.submitBudget(id, SecurityUtils.currentUsername());

        return new ApiResponse("success", "Budget submitted", null);
    }

    @TenantManagerOnly
    @PostMapping("/{id}/approve")
    public ApiResponse approve(@PathVariable UUID id) {

        budgetService.approveBudget(id, SecurityUtils.currentUsername());

        return new ApiResponse("success", "Budget approved", null);
    }
}