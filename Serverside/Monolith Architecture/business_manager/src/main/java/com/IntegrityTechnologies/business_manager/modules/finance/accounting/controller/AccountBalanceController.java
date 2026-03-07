package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountBalance;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.AccountBalanceResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
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

    @GetMapping
    public Page<AccountBalanceResponse> all(
            @RequestParam UUID branchId,
            Pageable pageable
    ) {
        return repository
                .findByBranch_Id(branchId, pageable)
                .map(AccountBalanceResponse::from);
    }

    @GetMapping("/account/{accountId}")
    public Page<AccountBalance> byAccount(
            @PathVariable UUID accountId,
            @RequestParam UUID branchId,
            @PageableDefault(size = 50, sort = "updatedAt") Pageable pageable
    ) {
        return repository.findByAccount_IdAndBranch_Id(accountId, branchId, pageable);
    }
}