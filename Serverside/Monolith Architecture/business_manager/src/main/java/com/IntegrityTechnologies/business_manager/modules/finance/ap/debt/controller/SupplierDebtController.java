package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.controller;

import com.IntegrityTechnologies.business_manager.config.response.PageWrapper;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.dto.SupplierDebtSummaryDto;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.dto.SupplierWorkspaceDto;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.service.SupplierDebtSummaryService;
import com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.service.SupplierWorkspaceService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/finance/ap/debts")
@RequiredArgsConstructor
@TenantManagerOnly
public class SupplierDebtController {

    private final SupplierDebtSummaryService service;
    private final SupplierWorkspaceService workspaceService;

    @GetMapping
    public PageWrapper<SupplierDebtSummaryDto> getDebtSummary(
            @RequestParam UUID branchId,
            @RequestParam(required = false) String search,
            @RequestParam(required = false) Boolean hasOverdue,
            @RequestParam(required = false) Boolean hasUnapplied,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size,
            @RequestParam(defaultValue = "supplierName") String sortBy,
            @RequestParam(defaultValue = "asc") String direction
    ) {
        sortBy = whitelistSort(sortBy);
        direction =
                "desc".equalsIgnoreCase(direction)
                        ? "desc"
                        : "asc";
        Pageable pageable =
                PageRequest.of(
                        page,
                        size,
                        Sort.by(
                                Sort.Direction.fromString(direction),
                                sortBy
                        )
                );
        return new PageWrapper<>(
                service.getDebtSummary(
                        branchId,
                        search,
                        hasOverdue,
                        hasUnapplied,
                        pageable
                )
        );
    }

    private String whitelistSort(String sortBy) {

        return switch (sortBy) {

            case "supplierName",
                    "totalOutstanding",
                    "overdueAmount",
                    "unappliedPayments",
                    "netPayable",
                    "openBills",
                    "overdueBills",
                    "oldestDueDate",
                    "lastPaymentDate"
                    -> sortBy;

            default -> "supplierName";
        };
    }

    @GetMapping("/{supplierId}")
    public SupplierWorkspaceDto getWorkspace(
            @RequestParam UUID branchId,
            @PathVariable UUID supplierId
    ) {
        return workspaceService.getWorkspace(
                branchId,
                supplierId
        );
    }
}