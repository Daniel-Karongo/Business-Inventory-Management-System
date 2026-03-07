package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.ReconciliationItemResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.ReconciliationResult;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.ReconciliationItem;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.ReconciliationItemRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.ReconciliationState;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.ReconciliationStateRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.BalanceReconciliationService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.security.BranchResolver;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/accounting/reconciliation")
@RequiredArgsConstructor
@TenantManagerOnly
public class AccountingReconciliationController {

    private final BalanceReconciliationService service;
    private final ReconciliationStateRepository stateRepository;
    private final ReconciliationItemRepository itemRepo;
    private final BranchResolver branchResolver;

    @TenantAdminOnly
    @PostMapping("/run")
    public UUID run(
            @RequestParam UUID branchId,
            @RequestParam(defaultValue = "false") boolean repair
    ) {

        UUID effectiveBranchId = branchResolver.resolveBranch(branchId);
        String user = SecurityUtils.currentUsername();

        return service.runAndPersist(effectiveBranchId, repair, user);
    }

    @GetMapping("/results")
    public Page<ReconciliationItemResponse> results(
            @RequestParam UUID runId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "200") int size
    ) {

        return itemRepo
                .findByRun_Id(runId, PageRequest.of(page, size))
                .map(ReconciliationItemResponse::from);
    }

    @TenantAdminOnly
    @PostMapping
    public ReconciliationResult reconcile(
            @RequestParam UUID accountId,
            @RequestParam UUID branchId,
            @RequestParam(defaultValue = "false") boolean repair
    ) {

        UUID effectiveBranchId = branchResolver.resolveBranch(branchId);
        String user = SecurityUtils.currentUsername();

        return service.reconcileAccount(accountId, effectiveBranchId, repair, user);
    }

    @TenantManagerOnly
    @GetMapping("/status")
    public ReconciliationStatus status(@RequestParam UUID branchId) {

        UUID effectiveBranchId = branchResolver.resolveBranch(branchId);
        ReconciliationState state =
                stateRepository.findByBranchId(effectiveBranchId)
                        .orElse(null);

        if (state == null) {
            return null;
        }

        return new ReconciliationStatus(
                state.getLastRunAt(),
                state.getInconsistenciesDetected(),
                state.isAutoRepairEnabled()
        );
    }

    @GetMapping("/results/mismatches")
    public Page<ReconciliationItem> mismatches(
            @RequestParam UUID runId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "200") int size
    ) {
        return itemRepo.findByRun_IdAndConsistentFalse(
                runId,
                PageRequest.of(page, size)
        );
    }

    @TenantAdminOnly
    @PostMapping("/auto-repair")
    public void toggleAutoRepair(
            @RequestParam UUID branchId,
            @RequestParam boolean enabled
    ) {

        UUID effectiveBranchId = branchResolver.resolveBranch(branchId);
        String user = SecurityUtils.currentUsername();

        service.setAutoRepair(effectiveBranchId, enabled, user);
    }

    public record ReconciliationStatus(
            java.time.LocalDateTime lastRunAt,
            long inconsistenciesDetected,
            boolean autoRepairEnabled
    ) {}
}