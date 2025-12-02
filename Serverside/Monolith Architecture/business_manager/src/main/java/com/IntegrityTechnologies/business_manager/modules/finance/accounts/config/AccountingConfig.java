package com.IntegrityTechnologies.business_manager.modules.finance.accounts.config;

import com.IntegrityTechnologies.business_manager.modules.finance.accounts.model.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.model.Account.AccountType;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.AccountingService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.JournalService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.PartTransactionService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.impl.DoubleEntryAccountingService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.impl.SingleEntryAccountingService;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.math.BigDecimal;
import java.util.UUID;

@Configuration
@RequiredArgsConstructor
public class AccountingConfig {

    private final JournalService journalService;
    private final AccountRepository accountRepository;
    private final PartTransactionService partTransactionService;

    @Value("${accounting.mode:DOUBLE_ENTRY}")
    private String accountingMode;

    @Bean
    public AccountingService accountingService() {
        String mode = accountingMode.toUpperCase();
        if ("SINGLE_ENTRY".equals(mode)) {
            return new SingleEntryAccountingService(accountRepository, partTransactionService);
        }
        return new DoubleEntryAccountingService(journalService, accountRepository, partTransactionService);
    }

    @Bean
    public CommandLineRunner seedDefaultAccounts(AccountRepository repo) {
        return args -> {
            ensureAccount(repo, "1000", "Cash Account", AccountType.ASSET);
            ensureAccount(repo, "4000", "Sales Revenue", AccountType.INCOME);
            ensureAccount(repo, "1200", "Inventory", AccountType.ASSET);
            ensureAccount(repo, "2000", "Accounts Payable", AccountType.LIABILITY);
            ensureAccount(repo, "WALLET", "Wallet Account", AccountType.ASSET);
        };
    }

    private void ensureAccount(AccountRepository repo, String code, String name, AccountType type) {
        if (!repo.existsByCode(code)) {
            Account a = new Account();
            a.setCode(code);
            a.setName(name);
            a.setType(type);
            a.setBalance(BigDecimal.ZERO);
            a.setActive(true);
            repo.save(a);
        }
    }
}