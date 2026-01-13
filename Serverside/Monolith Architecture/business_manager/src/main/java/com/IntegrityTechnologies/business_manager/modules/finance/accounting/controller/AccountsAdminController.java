package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.CreateAccountRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.UpdateAccountRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/accounts/admin")
@RequiredArgsConstructor
public class AccountsAdminController {

    private final AccountRepository accountRepository;
    private final LedgerEntryRepository ledgerRepo;

    /* ============================================================
       CREATE
    ============================================================ */
    @PostMapping
    public Account create(@RequestBody CreateAccountRequest req) {

        SecurityUtils.requireAdmin();

        accountRepository.findByCode(req.code())
                .ifPresent(a -> {
                    throw new IllegalStateException("Account code already exists");
                });

        return accountRepository.save(
                new Account(req.code(), req.name(), req.type())
        );
    }

    /* ============================================================
       RENAME (SAFE)
    ============================================================ */
    @PatchMapping("/{id}")
    public Account rename(
            @PathVariable UUID id,
            @RequestBody UpdateAccountRequest req
    ) {
        SecurityUtils.requireAdmin();

        Account acc = accountRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Account not found"));

        acc.setName(req.name());
        return accountRepository.save(acc);
    }

    /* ============================================================
       DEACTIVATE (ONLY IF UNUSED)
    ============================================================ */
    @PostMapping("/{id}/deactivate")
    public void deactivate(@PathVariable UUID id) {

        SecurityUtils.requireAdmin();

        Account acc = accountRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Account not found"));

        if (ledgerRepo.existsByAccountId(id)) {
            throw new IllegalStateException("Account has ledger entries");
        }

        acc.setActive(false);
        accountRepository.save(acc);
    }
}