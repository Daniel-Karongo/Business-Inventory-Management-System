package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountingMode;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.AccountingSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantAdminOnly;
import com.IntegrityTechnologies.business_manager.security.BranchResolver;
import com.IntegrityTechnologies.business_manager.security.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.UUID;

@RestController
@RequestMapping("/api/accounting/mode")
@RequiredArgsConstructor
@TenantAdminOnly
public class AccountingModeController {

    private final AccountingSystemStateService stateService;
    private final BranchResolver branchResolver;
    private final BranchTenantGuard branchTenantGuard;

    @PostMapping("/update")
    public void updateMode(
            @RequestParam(required = false) UUID branchId,
            @RequestParam AccountingMode mode
    ) {
        branchTenantGuard.validate(branchId);
        UUID effectiveBranchId = branchResolver.resolveBranch(branchId);
        String user = SecurityUtils.currentUsername();

        stateService.updateMode(effectiveBranchId, mode, user);
    }
}