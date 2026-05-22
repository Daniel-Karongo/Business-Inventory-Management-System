package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.projection.AccountListItemProjection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/accounts")
@RequiredArgsConstructor
@TenantManagerOnly
public class AccountsController {

    private final AccountRepository accountRepository;
    private final BranchTenantGuard branchTenantGuard;

    @GetMapping
    public Page<AccountListItemProjection> listAccounts(
            @RequestParam UUID branchId,
            @RequestParam(required = false) String search,
            @RequestParam(required = false) AccountType type,
            @RequestParam(required = false) Boolean active,
            @RequestParam(required = false) String balanceFilter,
            Pageable pageable
    ) {
        branchTenantGuard.validate(branchId);

        return accountRepository.findAccountListItems(
                TenantContext.getTenantId(),
                branchId,
                search,
                type,
                active,
                balanceFilter,
                pageable
        );
    }

    @GetMapping("/{id}")
    public AccountResponse get(@PathVariable UUID id) {
        Account a = accountRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Account not found"));
        branchTenantGuard.validate(a.getBranchId());
        return AccountResponse.from(a);
    }

    record AccountResponse(
            String id,
            String code,
            String name,
            String type
    ) {
        static AccountResponse from(Account a) {
            return new AccountResponse(
                    a.getId().toString(),
                    a.getCode(),
                    a.getName(),
                    a.getType().name()
            );
        }
    }
}