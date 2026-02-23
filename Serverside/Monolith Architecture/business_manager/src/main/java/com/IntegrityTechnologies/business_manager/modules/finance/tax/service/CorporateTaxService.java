package com.IntegrityTechnologies.business_manager.modules.finance.tax.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.config.TaxProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.CorporateTaxFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.enums.BusinessTaxMode;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.CorporateTaxFilingRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

import static com.IntegrityTechnologies.business_manager.modules.finance.accounting.support.AccountingSignRules.*;

@Service
@RequiredArgsConstructor
public class CorporateTaxService {

    private final LedgerEntryRepository ledgerRepository;
    private final CorporateTaxFilingRepository filingRepository;
    private final AccountingFacade accountingFacade;
    private final AccountingAccounts accounts;
    private final TaxProperties taxProperties;

    @Transactional
    public CorporateTaxFiling accrueCorporateTax(
            UUID periodId,
            LocalDateTime from,
            LocalDateTime to,
            String user
    ) {
        if (taxProperties.getBusinessTaxMode() != BusinessTaxMode.CORPORATE) {
            throw new IllegalStateException(
                    "Corporate tax accrual disabled. Business tax mode is "
                            + taxProperties.getBusinessTaxMode()
            );
        }

        BigDecimal revenue = ledgerRepository.netMovementForAccount(
                accounts.revenue(),
                from,
                to,
                DEBIT_NORMAL,
                CREDIT_NORMAL,
                DEBIT,
                CREDIT
        );

        BigDecimal cogs = ledgerRepository.netMovementForAccount(
                accounts.cogs(),
                from,
                to,
                DEBIT_NORMAL,
                CREDIT_NORMAL,
                DEBIT,
                CREDIT
        );

        BigDecimal expenses = ledgerRepository.totalExpensesBetween(
                from,
                to,
                AccountType.EXPENSE,
                EntryDirection.DEBIT
        );

        BigDecimal profit =
                revenue.subtract(cogs).subtract(expenses);

        if (profit.compareTo(BigDecimal.ZERO) <= 0) {
            return CorporateTaxFiling.builder()
                    .periodId(periodId)
                    .taxableProfit(profit)
                    .taxRate(taxProperties.getCorporateTaxRate())
                    .taxAmount(BigDecimal.ZERO)
                    .filedBy(user)
                    .filedAt(LocalDateTime.now())
                    .paid(false)
                    .build();
        }

        BigDecimal taxAmount =
                profit.multiply(taxProperties.getCorporateTaxRate());

        accountingFacade.post(
                AccountingEvent.builder()
                        .sourceModule("CORPORATE_TAX")
                        .sourceId(periodId)
                        .reference("CTAX-" + periodId)
                        .description("Corporate tax accrual")
                        .performedBy(user)
                        .entries(
                                java.util.List.of(
                                        AccountingEvent.Entry.builder()
                                                .accountId(accounts.corporateTaxExpense())
                                                .direction(EntryDirection.DEBIT)
                                                .amount(taxAmount)
                                                .build(),

                                        AccountingEvent.Entry.builder()
                                                .accountId(accounts.corporateTaxPayable())
                                                .direction(EntryDirection.CREDIT)
                                                .amount(taxAmount)
                                                .build()
                                )
                        )
                        .build()
        );

        CorporateTaxFiling filing = CorporateTaxFiling.builder()
                .periodId(periodId)
                .taxableProfit(profit)
                .taxRate(taxProperties.getCorporateTaxRate())
                .taxAmount(taxAmount)
                .filedBy(user)
                .filedAt(LocalDateTime.now())
                .paid(false)
                .build();

        return filingRepository.save(filing);
    }

    @Transactional
    public void markPaid(UUID filingId, String user, UUID paymentAccount) {
        if (taxProperties.getBusinessTaxMode() != BusinessTaxMode.CORPORATE) {
            throw new IllegalStateException(
                    "Corporate tax payment disabled. Business tax mode is "
                            + taxProperties.getBusinessTaxMode()
            );
        }

        CorporateTaxFiling filing =
                filingRepository.findById(filingId)
                        .orElseThrow();

        if (filing.isPaid()) return;

        accountingFacade.post(
                AccountingEvent.builder()
                        .sourceModule("CORPORATE_TAX_PAYMENT")
                        .sourceId(filingId)
                        .reference("CTAX-PAY-" + filingId)
                        .description("Corporate tax payment")
                        .performedBy(user)
                        .entries(
                                java.util.List.of(
                                        AccountingEvent.Entry.builder()
                                                .accountId(accounts.corporateTaxPayable())
                                                .direction(EntryDirection.DEBIT)
                                                .amount(filing.getTaxAmount())
                                                .build(),

                                        AccountingEvent.Entry.builder()
                                                .accountId(paymentAccount)
                                                .direction(EntryDirection.CREDIT)
                                                .amount(filing.getTaxAmount())
                                                .build()
                                )
                        )
                        .build()
        );

        filing.setPaid(true);
        filing.setPaidAt(LocalDateTime.now());
        filingRepository.save(filing);
    }

    public List<CorporateTaxFiling> listAll() {
        return filingRepository.findAll();
    }
}