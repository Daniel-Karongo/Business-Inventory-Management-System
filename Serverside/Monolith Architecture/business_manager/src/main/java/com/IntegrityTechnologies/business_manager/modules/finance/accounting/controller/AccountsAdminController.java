package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.CreateAccountRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.UpdateAccountRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantAdminOnly;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/accounts/admin")
@RequiredArgsConstructor
@TenantAdminOnly
public class AccountsAdminController {

    private final AccountRepository accountRepository;
    private final LedgerEntryRepository ledgerRepo;

    @PostMapping
    public Account create(@RequestBody CreateAccountRequest req) {

        accountRepository
                .findByBranchIdAndCode(req.branchId(), req.code())
                .ifPresent(a -> {
                    throw new IllegalStateException("Account code already exists");
                });

        return accountRepository.save(
                new Account(
                        req.branchId(),
                        req.code(),
                        req.name(),
                        req.type(),
                        req.role()
                )
        );
    }

    @PatchMapping("/{id}")
    public Account rename(
            @PathVariable UUID id,
            @RequestBody UpdateAccountRequest req
    ) {

        Account acc = accountRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Account not found"));

        acc.setName(req.name());
        return accountRepository.save(acc);
    }

    @PostMapping("/{id}/deactivate")
    public void deactivate(@PathVariable UUID id) {

        Account acc = accountRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Account not found"));

        if (ledgerRepo.existsByAccount_Id(id)) {
            throw new IllegalStateException("Account has ledger entries");
        }

        acc.setActive(false);
        accountRepository.save(acc);
    }
}