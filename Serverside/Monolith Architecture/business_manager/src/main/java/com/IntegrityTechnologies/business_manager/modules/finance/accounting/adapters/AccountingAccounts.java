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

    public UUID inventory() {
        return accountRepository.findByCode("1200")
                .orElseThrow().getId();
    }

    public UUID cogs() {
        return accountRepository.findByCode("5000")
                .orElseThrow().getId();
    }

    public UUID accountsPayable() {
        return accountRepository.findByCode("2000")
                .orElseThrow().getId();
    }

    public UUID accountsReceivable() {
        return accountRepository.findByCode("1500")
                .orElseThrow(() -> new IllegalStateException("Accounts Receivable missing"))
                .getId();
    }

    public UUID inputVat() {
        return accountRepository.findByCode("1300")
                .orElseThrow(() -> new IllegalStateException("Input VAT account missing"))
                .getId();
    }

    public UUID outputVat() {
        return accountRepository.findByCode("2100")
                .orElseThrow(() -> new IllegalStateException("Output VAT account missing"))
                .getId();
    }

    public UUID vatPayable() {
        return accountRepository.findByCode("2200")
                .orElseThrow(() -> new IllegalStateException("VAT Payable account missing"))
                .getId();
    }

    public UUID mpesa() {
        return accountRepository.findByCode("1150")
                .orElseThrow(() -> new IllegalStateException("M-Pesa account missing"))
                .getId();
    }

    public UUID corporateTaxPayable() {
        return accountRepository.findByCode("2400")
                .orElseThrow(() -> new IllegalStateException("Corporate Tax Payable missing"))
                .getId();
    }

    public UUID corporateTaxExpense() {
        return accountRepository.findByCode("5100")
                .orElseThrow(() -> new IllegalStateException("Corporate Tax Expense missing"))
                .getId();
    }
}