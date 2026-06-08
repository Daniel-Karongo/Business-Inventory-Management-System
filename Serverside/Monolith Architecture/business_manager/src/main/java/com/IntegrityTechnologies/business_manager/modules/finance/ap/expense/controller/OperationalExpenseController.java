package com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.controller;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.domain.ExpenseStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.dto.BulkSettleOperationalExpenseRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.dto.CreateOperationalExpenseRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.dto.SettleOperationalExpenseRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.service.OperationalExpenseService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/operational-expenses")
@RequiredArgsConstructor
public class OperationalExpenseController {

    private final OperationalExpenseService service;

    @GetMapping("/branch/{branchId}/open")
    public ApiResponse open(
            @PathVariable UUID branchId
    ) {

        return new ApiResponse(
                "success",
                "Open expenses",
                service.getOpenExpenses(
                        branchId
                )
        );
    }

    @PostMapping
    public ApiResponse create(
            @RequestBody
            CreateOperationalExpenseRequest request
    ) {
        return new ApiResponse(
                "success",
                "Expense created",
                service.createExpense(
                        request
                )
        );
    }

    @GetMapping("/branch/{branchId}")
    public ApiResponse list(
            @PathVariable UUID branchId,
            Pageable pageable,
            @RequestParam(required = false)
            String search,
            @RequestParam(required = false)
            ExpenseStatus status
    ) {
        return new ApiResponse(
                "success",
                "Expenses",
                service.list(
                        branchId,
                        pageable,
                        search,
                        status
                )
        );
    }

    @GetMapping("/branch/{branchId}/{expenseId}")
    public ApiResponse workspace(
            @PathVariable UUID branchId,
            @PathVariable UUID expenseId
    ) {
        return new ApiResponse(
                "success",
                "Expense workspace",
                service.workspace(
                        branchId,
                        expenseId
                )
        );
    }

    @PostMapping("/{branchId}/{expenseId}/settle")
    public ApiResponse settle(
            @PathVariable UUID branchId,
            @PathVariable UUID expenseId,
            @RequestBody SettleOperationalExpenseRequest request
    ) {

        return new ApiResponse(
                "success",
                "Expense settled",
                service.settleExpense(
                        branchId,
                        expenseId,
                        request
                )
        );
    }

    @PostMapping("/{branchId}/bulk-settle")
    public ApiResponse bulkSettle(
            @PathVariable UUID branchId,
            @RequestBody
            BulkSettleOperationalExpenseRequest request
    ) {

        service.bulkSettle(
                branchId,
                request
        );

        return new ApiResponse(
                "success",
                "Expenses settled",
                null
        );
    }

    @PostMapping("/{branchId}/{expenseId}/reverse")
    public ApiResponse reverseExpense(
            @PathVariable UUID branchId,
            @PathVariable UUID expenseId,
            @RequestParam String reason
    ) {

        service.reverseExpense(
                branchId,
                expenseId,
                reason
        );

        return new ApiResponse(
                "success",
                "Expense reversed",
                null
        );
    }

    @PostMapping("/{branchId}/settlements/{settlementId}/reverse")
    public ApiResponse reverseSettlement(
            @PathVariable UUID branchId,
            @PathVariable UUID settlementId,
            @RequestParam String reason
    ) {

        service.reverseSettlement(
                branchId,
                settlementId,
                reason
        );

        return new ApiResponse(
                "success",
                "Settlement reversed",
                null
        );
    }

}