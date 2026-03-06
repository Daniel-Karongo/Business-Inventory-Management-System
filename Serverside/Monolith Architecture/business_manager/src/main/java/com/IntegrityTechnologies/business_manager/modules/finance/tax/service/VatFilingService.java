package com.IntegrityTechnologies.business_manager.modules.finance.tax.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.TaxPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.VatFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.VatFilingRepository;
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
    public VatFiling file(TaxPeriod period, UUID branchId, String user) {

        if (branchId == null) {
            throw new IllegalArgumentException("BranchId required for VAT filing.");
        }

        if (filingRepo.existsByPeriod_IdAndBranchId(period.getId(), branchId)) {
            throw new IllegalStateException("VAT already filed for this period and branch.");
        }

        if (!period.isClosed()) {

            throw new IllegalStateException(
                    "VAT filing allowed only after tax period is closed."
            );
        }

        var report = reportService.generate(
                period.getStartDate().atStartOfDay(),
                period.getEndDate().atTime(23,59,59),
                branchId
        );

        BigDecimal outputVat = report.outputVat();
        BigDecimal inputVat = report.inputVat();
        BigDecimal payable = outputVat.subtract(inputVat);

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(UUID.randomUUID())
                        .sourceId(period.getId())
                        .sourceModule("VAT_CLEARING")
                        .reference("VAT-CLEAR-" + period.getId() + "-" + branchId)
                        .description("VAT clearing for period")
                        .performedBy(user)
                        .branchId(branchId)
                        .entries(buildVatClearingEntries(branchId, outputVat, inputVat, payable))
                        .build()
        );

        VatFiling filing = VatFiling.builder()
                .period(period)
                .branchId(branchId)
                .outputVat(outputVat)
                .inputVat(inputVat)
                .vatPayable(payable)
                .filedBy(user)
                .filedAt(LocalDateTime.now())
                .paid(false)
                .build();

        filingRepo.save(filing);

        return filing;
    }

    private List<AccountingEvent.Entry> buildVatClearingEntries(
            UUID branchId,
            BigDecimal outputVat,
            BigDecimal inputVat,
            BigDecimal payable
    ) {

        List<AccountingEvent.Entry> entries = new ArrayList<>();

        if (outputVat.compareTo(BigDecimal.ZERO) > 0) {
            entries.add(AccountingEvent.Entry.builder()
                    .accountId(accounts.get(branchId, AccountRole.VAT_OUTPUT))
                    .direction(EntryDirection.DEBIT)
                    .amount(outputVat)
                    .build());
        }

        if (inputVat.compareTo(BigDecimal.ZERO) > 0) {
            entries.add(AccountingEvent.Entry.builder()
                    .accountId(accounts.get(branchId, AccountRole.VAT_INPUT))
                    .direction(EntryDirection.CREDIT)
                    .amount(inputVat)
                    .build());
        }

        if (payable.compareTo(BigDecimal.ZERO) > 0) {
            entries.add(AccountingEvent.Entry.builder()
                    .accountId(accounts.get(branchId, AccountRole.VAT_PAYABLE))
                    .direction(EntryDirection.CREDIT)
                    .amount(payable)
                    .build());
        }

        return entries;
    }

    @Transactional
    public void markPaid(VatFiling filing, String user, UUID paymentAccountId) {

        if (filing.isPaid()) {

            throw new IllegalStateException(
                    "VAT filing already paid and locked."
            );
        }

        UUID branchId = filing.getBranchId();

        BigDecimal amount = filing.getVatPayable();

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(UUID.randomUUID())
                        .sourceId(filing.getId())
                        .sourceModule("VAT_PAYMENT")
                        .reference("VAT-PAY-" + filing.getId())
                        .description("VAT payment for period")
                        .performedBy(user)
                        .branchId(branchId)
                        .entries(List.of(
                                AccountingEvent.Entry.builder()
                                        .accountId(accounts.get(branchId, AccountRole.VAT_PAYABLE))
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