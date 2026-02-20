package com.IntegrityTechnologies.business_manager.modules.finance.accounting.seed;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@RequiredArgsConstructor
public class ChartOfAccountsSeeder {

    private final AccountRepository repo;

    @Bean
    CommandLineRunner seedChart() {
        return args -> {
            seed("1000", "Cash", AccountType.ASSET);
            seed("1100", "Bank", AccountType.ASSET);
            seed("1200", "Inventory", AccountType.ASSET);
            seed("1300", "Input VAT", AccountType.ASSET);
            seed("1150", "M-Pesa Clearing Account", AccountType.ASSET);
            seed("1500", "Accounts Receivable", AccountType.ASSET);

            seed("2000", "Accounts Payable", AccountType.LIABILITY);
            seed("2100", "Output VAT", AccountType.LIABILITY);
            seed("2200", "VAT Payable", AccountType.LIABILITY);
            seed("2400", "Corporate Tax Payable", AccountType.LIABILITY);

            seed("3000", "Owner Equity", AccountType.EQUITY);

            seed("4000", "Sales Revenue", AccountType.INCOME);
            seed("5000", "Cost of Goods Sold", AccountType.EXPENSE);
            seed("5100", "Corporate Tax Expense", AccountType.EXPENSE);
        };
    }

    private void seed(String code, String name, AccountType type) {
        repo.findByCode(code)
                .orElseGet(() -> repo.save(new Account(code, name, type)));
    }
}