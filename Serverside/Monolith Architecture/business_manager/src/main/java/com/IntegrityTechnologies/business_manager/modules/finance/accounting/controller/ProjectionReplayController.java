package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.replay.FinancialProjectionReplayService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantAdminOnly;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/accounting/replay/projections")
@RequiredArgsConstructor
@TenantAdminOnly
public class ProjectionReplayController {

    private final FinancialProjectionReplayService replayService;
    private final BranchTenantGuard branchTenantGuard;

    @PostMapping("/branch/{branchId}")
    public void rebuildBranch(@PathVariable UUID branchId) {
        branchTenantGuard.validate(branchId);
        replayService.replayBranch(branchId);
    }
}