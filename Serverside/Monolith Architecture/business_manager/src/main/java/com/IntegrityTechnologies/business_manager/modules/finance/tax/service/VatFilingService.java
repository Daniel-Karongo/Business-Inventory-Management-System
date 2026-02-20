package com.IntegrityTechnologies.business_manager.modules.finance.tax.service;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.*;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.VatFilingRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class VatFilingService {

    private final VatReportService reportService;
    private final VatFilingRepository filingRepo;
    private final AccountingFacade accountingFacade;
    private final AccountingAccounts accounts;

    @Transactional
    public VatFiling file(TaxPeriod period, String user) {

        var report = reportService.generate(
                period.getStartDate().atStartOfDay(),
                period.getEndDate().atTime(23,59,59)
        );

        BigDecimal outputVat = report.outputVat();
        BigDecimal inputVat = report.inputVat();
        BigDecimal payable = outputVat.subtract(inputVat);

        // -----------------------------------
        // VAT CLEARING JOURNAL
        // -----------------------------------
        accountingFacade.post(
                AccountingEvent.builder()
                        .sourceModule("VAT_CLEARING")
                        .reference("VAT-CLEAR-" + period.getId())
                        .description("VAT clearing for period")
                        .performedBy(user)
                        .entries(buildVatClearingEntries(
                                outputVat,
                                inputVat,
                                payable
                        ))
                        .build()
        );

        VatFiling filing = VatFiling.builder()
                .period(period)
                .outputVat(outputVat)
                .inputVat(inputVat)
                .vatPayable(payable)
                .filedBy(user)
                .filedAt(LocalDateTime.now())
                .paid(false)
                .build();

        filingRepo.save(filing);

        period.setClosed(true);

        return filing;
    }

    private List<AccountingEvent.Entry> buildVatClearingEntries(
            BigDecimal outputVat,
            BigDecimal inputVat,
            BigDecimal payable
    ) {

        List<AccountingEvent.Entry> entries = new ArrayList<>();

        // Clear Output VAT
        if (outputVat.compareTo(BigDecimal.ZERO) > 0) {
            entries.add(AccountingEvent.Entry.builder()
                    .accountId(accounts.outputVat())
                    .direction(EntryDirection.DEBIT)
                    .amount(outputVat)
                    .build());
        }

        // Clear Input VAT
        if (inputVat.compareTo(BigDecimal.ZERO) > 0) {
            entries.add(AccountingEvent.Entry.builder()
                    .accountId(accounts.inputVat())
                    .direction(EntryDirection.CREDIT)
                    .amount(inputVat)
                    .build());
        }

        // Move net to VAT Payable (if positive)
        if (payable.compareTo(BigDecimal.ZERO) > 0) {
            entries.add(AccountingEvent.Entry.builder()
                    .accountId(accounts.vatPayable())
                    .direction(EntryDirection.CREDIT)
                    .amount(payable)
                    .build());
        }

        return entries;
    }

    @Transactional
    public void markPaid(VatFiling filing, String user, UUID paymentAccountId) {

        if (filing.isPaid()) return;

        if (paymentAccountId == null) {
            throw new IllegalArgumentException("paymentAccountId is required");
        }

        BigDecimal amount = filing.getVatPayable();

        accountingFacade.post(
                AccountingEvent.builder()
                        .sourceModule("VAT_PAYMENT")
                        .reference("VAT-PAY-" + filing.getId())
                        .description("VAT payment for period")
                        .performedBy(user)
                        .entries(List.of(
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.vatPayable())
                                        .direction(EntryDirection.DEBIT)
                                        .amount(amount)
                                        .build(),

                                AccountingEvent.Entry.builder()
                                        .accountId(paymentAccountId)
                                        .direction(EntryDirection.CREDIT)
                                        .amount(amount)
                                        .build()
                        ))
                        .build()
        );

        filing.setPaid(true);
        filingRepo.save(filing);
    }
}