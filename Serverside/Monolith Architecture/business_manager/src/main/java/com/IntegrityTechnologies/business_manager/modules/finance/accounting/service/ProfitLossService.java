package com.IntegrityTechnologies.business_manager.modules.finance.accounting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.config.TaxProperties;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class ProfitLossService {

    private final LedgerEntryRepository ledgerRepo;
    private final AccountingAccounts accountingAccounts;
    private final TaxProperties taxProperties;

    public ProfitLossReport generate(LocalDateTime from, LocalDateTime to) {

        UUID revenueId = accountingAccounts.revenue();
        UUID cogsId = accountingAccounts.cogs();

        List<LedgerEntry> entries =
                ledgerRepo.findByPostedAtBetween(from, to);

        BigDecimal revenue = entries.stream()
                .filter(e -> e.getAccount().getId().equals(revenueId))
                .filter(e -> e.getDirection() == EntryDirection.CREDIT)
                .map(LedgerEntry::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal cogs = entries.stream()
                .filter(e -> e.getAccount().getId().equals(cogsId))
                .filter(e -> e.getDirection() == EntryDirection.DEBIT)
                .map(LedgerEntry::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal grossProfit = revenue.subtract(cogs);

        BigDecimal corporateTax = grossProfit.compareTo(BigDecimal.ZERO) > 0
                ? grossProfit.multiply(taxProperties.getCorporateTaxRate())
                : BigDecimal.ZERO;

        BigDecimal netProfit = grossProfit.subtract(corporateTax);

        return new ProfitLossReport(
                revenue,
                cogs,
                grossProfit,
                corporateTax,
                netProfit
        );
    }

    public record ProfitLossReport(
            BigDecimal revenue,
            BigDecimal costOfGoodsSold,
            BigDecimal grossProfit,
            BigDecimal estimatedCorporateTax,
            BigDecimal netProfit
    ) {}
}