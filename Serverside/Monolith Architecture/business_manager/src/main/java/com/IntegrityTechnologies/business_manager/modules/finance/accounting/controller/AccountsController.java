package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/accounts")
@RequiredArgsConstructor
public class AccountsController {

    private final AccountRepository accountRepository;

    @GetMapping
    public List<AccountResponse> listAccounts() {
        return accountRepository.findAll().stream()
                .filter(Account::isActive)
                .map(AccountResponse::from)
                .toList();
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