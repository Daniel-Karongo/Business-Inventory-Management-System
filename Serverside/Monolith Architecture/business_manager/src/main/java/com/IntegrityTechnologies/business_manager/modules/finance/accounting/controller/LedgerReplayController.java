package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.replay.LedgerReplayService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/accounting/replay")
@RequiredArgsConstructor
@TenantManagerOnly
public class LedgerReplayController {

    private final LedgerReplayService replayService;

    @PostMapping("/branch/{branchId}")
    public void rebuildBranch(@PathVariable UUID branchId) {

        replayService.rebuildBranch(branchId);
    }
}