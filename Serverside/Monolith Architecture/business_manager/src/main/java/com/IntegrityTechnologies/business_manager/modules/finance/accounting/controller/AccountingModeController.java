package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountingMode;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.AccountingSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.security.BranchResolver;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/accounting/mode")
@RequiredArgsConstructor
public class AccountingModeController {

    private final AccountingSystemStateService stateService;
    private final BranchResolver branchResolver;

    @PostMapping("/update")
    public void updateMode(
            @RequestParam(required = false) UUID branchId,
            @RequestParam AccountingMode mode
    ) {
        SecurityUtils.requireAdmin();

        UUID effectiveBranchId = branchResolver.resolveBranch(branchId);
        String user = SecurityUtils.currentUsername();

        stateService.updateMode(effectiveBranchId, mode, user);
    }
}