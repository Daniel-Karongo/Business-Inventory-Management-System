package com.IntegrityTechnologies.business_manager.modules.finance.accounts.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounts.model.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.repository.AccountRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class AccountServiceImpl implements AccountService {

    private final AccountRepository repo;

    @Override
    @Transactional
    public Account createAccount(Account account) {
        if (account.getBalance() == null) account.setBalance(BigDecimal.ZERO);
        account.setActive(true);
        return repo.save(account);
    }

    @Override
    public Account getAccount(UUID id) {
        return repo.findById(id).orElseThrow(() -> new IllegalArgumentException("Account not found: " + id));
    }

    @Override
    public List<Account> listAccounts() {
        return repo.findAll();
    }
}