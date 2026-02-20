package com.IntegrityTechnologies.business_manager.modules.finance.tax.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class VatReportService {

    private final LedgerEntryRepository ledgerRepo;
    private final AccountingAccounts accountingAccounts;

    public VatReport generate(LocalDateTime from, LocalDateTime to) {

        UUID outputVatId = accountingAccounts.outputVat();
        UUID inputVatId = accountingAccounts.inputVat();

        List<LedgerEntry> entries =
                ledgerRepo.findByPostedAtBetween(from, to);

        BigDecimal outputVat = entries.stream()
                .filter(e -> e.getAccount().getId().equals(outputVatId))
                .filter(e -> e.getDirection() == EntryDirection.CREDIT)
                .map(LedgerEntry::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal inputVat = entries.stream()
                .filter(e -> e.getAccount().getId().equals(inputVatId))
                .filter(e -> e.getDirection() == EntryDirection.DEBIT)
                .map(LedgerEntry::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal payable = outputVat.subtract(inputVat);

        return new VatReport(outputVat, inputVat, payable);
    }

    public record VatReport(
            BigDecimal outputVat,
            BigDecimal inputVat,
            BigDecimal vatPayable
    ) {}
}