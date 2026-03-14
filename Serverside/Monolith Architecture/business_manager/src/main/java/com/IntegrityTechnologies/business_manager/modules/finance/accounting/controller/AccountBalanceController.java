package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountBalance;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.AccountBalanceResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.IntegrityTechnologies.business_manager.security.BranchTenantGuard;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/accounting/balances")
@RequiredArgsConstructor
@TenantManagerOnly
public class AccountBalanceController {

    private final AccountBalanceRepository repository;
    private final BranchTenantGuard branchTenantGuard;

    @GetMapping
    public Page<AccountBalanceResponse> all(
            @RequestParam UUID branchId,
            Pageable pageable
    ) {
        branchTenantGuard.validate(branchId);

        UUID tenantId = TenantContext.getTenantId();

        return repository
                .findByTenantIdAndBranch_Id(tenantId, branchId, pageable)
                .map(AccountBalanceResponse::from);
    }

    @GetMapping("/account/{accountId}")
    public Page<AccountBalance> byAccount(
            @PathVariable UUID accountId,
            @RequestParam UUID branchId,
            @PageableDefault(size = 50, sort = "updatedAt") Pageable pageable
    ) {
        branchTenantGuard.validate(branchId);

        UUID tenantId = TenantContext.getTenantId();

        return repository.findByTenantIdAndAccount_IdAndBranch_Id(
                tenantId,
                accountId,
                branchId,
                pageable
        );
    }
}