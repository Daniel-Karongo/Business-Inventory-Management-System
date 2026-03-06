package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.replay.FinancialProjectionReplayService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/accounting/replay/projections")
@RequiredArgsConstructor
public class ProjectionReplayController {

    private final FinancialProjectionReplayService replayService;

    @PostMapping("/branch/{branchId}")
    public void rebuildBranch(@PathVariable UUID branchId) {

        SecurityUtils.requireAtLeast(Role.ADMIN);

        replayService.replayBranch(branchId);
    }
}