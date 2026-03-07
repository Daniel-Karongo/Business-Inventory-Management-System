package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
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

    @GetMapping
    public Page<AccountResponse> listAccounts(
            @RequestParam UUID branchId,
            @PageableDefault(size = 50, sort = "code") Pageable pageable
    ) {
        return accountRepository.findByBranchIdAndActiveTrue(branchId, pageable)
                .map(AccountResponse::from);
    }

    @GetMapping("/{id}")
    public AccountResponse get(@PathVariable UUID id) {
        Account a = accountRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Account not found"));
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