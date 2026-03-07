package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.control.CloseChecklistService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.AccountingPeriodResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.GovernanceAuditService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountingPeriodRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.PeriodClosingService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.control.CloseChecklistService.CloseChecklistResult;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.UUID;

@RestController
@RequestMapping("/api/accounting/periods")
@RequiredArgsConstructor
@TenantManagerOnly
public class AccountingPeriodController {

    private final AccountingPeriodRepository repository;
    private final PeriodClosingService closingService;
    private final GovernanceAuditService auditService;
    private final CloseChecklistService checklistService;

    @TenantManagerOnly
    @GetMapping
    public Page<AccountingPeriodResponse> list(
            @RequestParam UUID branchId,
            @PageableDefault(size = 24, sort = "startDate") Pageable pageable
    ) {

        return repository
                .findByBranchId(branchId, pageable)
                .map(AccountingPeriodResponse::from);
    }

    @TenantAdminOnly
    @PostMapping("/{id}/close")
    public void close(@PathVariable UUID id) {

        closingService.closePeriod(
                id,
                SecurityUtils.currentUsername()
        );
    }

    @TenantManagerOnly
    @GetMapping("/close-checklist")
    public CloseChecklistResult previewClose(
            @RequestParam UUID periodId
    ) {

        AccountingPeriod period =
                repository.findById(periodId)
                        .orElseThrow();

        return checklistService.validate(
                period.getBranchId(),
                period
        );
    }

    @TenantAdminOnly
    @PostMapping("/{id}/reopen")
    public void reopen(
            @PathVariable UUID id,
            @RequestParam String reason
    ) {

        AccountingPeriod period =
                repository.findById(id)
                        .orElseThrow(() ->
                                new IllegalArgumentException("Period not found")
                        );

        if (!period.isClosed()) {
            throw new IllegalStateException("Period is already open");
        }

        boolean laterClosedExists =
                repository.findByBranchId(period.getBranchId())
                        .stream()
                        .anyMatch(p ->
                                p.isClosed() &&
                                        p.getStartDate().isAfter(period.getStartDate())
                        );

        if (laterClosedExists) {

            throw new IllegalStateException(
                    "Cannot reopen. A later period is already closed."
            );
        }

        period.setClosed(false);
        period.setReopenedBy(SecurityUtils.currentUsername());
        period.setReopenedAt(LocalDateTime.now());
        period.setReopenReason(reason);

        repository.save(period);

        auditService.log(
                period.getBranchId(),
                "PERIOD_REOPENED",
                SecurityUtils.currentUsername(),
                "Period ID: " + id + " | Reason: " + reason
        );
    }
}