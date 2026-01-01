package com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
@RequiredArgsConstructor
public class AccountingAccounts {

    private final AccountRepository accountRepository;

    public UUID cash() {
        return accountRepository.findByCode("1000")
                .orElseThrow(() -> new IllegalStateException("Cash account missing"))
                .getId();
    }

    public UUID bank() {
        return accountRepository.findByCode("1100")
                .orElseThrow(() -> new IllegalStateException("Bank account missing"))
                .getId();
    }

    public UUID revenue() {
        return accountRepository.findByCode("4000")
                .orElseThrow(() -> new IllegalStateException("Revenue account missing"))
                .getId();
    }
}