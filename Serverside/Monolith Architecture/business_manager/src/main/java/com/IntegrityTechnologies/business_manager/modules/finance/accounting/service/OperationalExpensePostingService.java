package com.IntegrityTechnologies.business_manager.modules.finance.accounting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class OperationalExpensePostingService {

    private final AccountingFacade accountingFacade;
    private final AccountingAccounts accountingAccounts;

    @Transactional
    public UUID postAccrualExpense(
            UUID branchId,
            UUID expenseAccountId,
            String reference,
            String description,
            BigDecimal amount,
            LocalDate accountingDate,
            UUID sourceId
    ) {

        UUID tenantId =
                TenantContext.getTenantId();

        UUID payableAccountId =
                accountingAccounts.get(
                        tenantId,
                        branchId,
                        "OPERATIONAL_EXPENSE_PAYABLE"
                );

        UUID eventId =
                UUID.nameUUIDFromBytes(
                        (
                                "OP_EXP_ACCRUAL:"
                                        + sourceId
                        ).getBytes()
                );

        JournalEntry journal =
                accountingFacade.post(
                        AccountingEvent.builder()
                                .eventId(eventId)
                                .tenantId(tenantId)
                                .branchId(branchId)
                                .sourceModule("OPERATIONAL_EXPENSE")
                                .sourceId(sourceId)
                                .reference(reference)
                                .description(description)
                                .performedBy("SYSTEM")
                                .accountingDate(accountingDate)
                                .entries(
                                        List.of(
                                                AccountingEvent.Entry.builder()
                                                        .accountId(expenseAccountId)
                                                        .direction(EntryDirection.DEBIT)
                                                        .amount(amount)
                                                        .build(),

                                                AccountingEvent.Entry.builder()
                                                        .accountId(payableAccountId)
                                                        .direction(EntryDirection.CREDIT)
                                                        .amount(amount)
                                                        .build()
                                        )
                                )
                                .build()
                );

        return journal.getId();
    }

    @Transactional
    public UUID postPaidExpense(
            UUID branchId,
            UUID fundingAccountId,
            UUID expenseAccountId,
            String reference,
            String description,
            BigDecimal amount,
            LocalDate accountingDate,
            UUID sourceId
    ) {

        UUID tenantId =
                TenantContext.getTenantId();

        UUID eventId =
                UUID.nameUUIDFromBytes(
                        (
                                "OP_EXP_PAID:"
                                        + sourceId
                        ).getBytes()
                );

        JournalEntry journal =
                accountingFacade.post(
                        AccountingEvent.builder()
                                .eventId(eventId)
                                .tenantId(tenantId)
                                .branchId(branchId)
                                .sourceModule("OPERATIONAL_EXPENSE")
                                .sourceId(sourceId)
                                .reference(reference)
                                .description(description)
                                .performedBy("SYSTEM")
                                .accountingDate(accountingDate)
                                .entries(
                                        List.of(
                                                AccountingEvent.Entry.builder()
                                                        .accountId(expenseAccountId)
                                                        .direction(EntryDirection.DEBIT)
                                                        .amount(amount)
                                                        .build(),

                                                AccountingEvent.Entry.builder()
                                                        .accountId(fundingAccountId)
                                                        .direction(EntryDirection.CREDIT)
                                                        .amount(amount)
                                                        .build()
                                        )
                                )
                                .build()
                );

        return journal.getId();
    }

    @Transactional
    public UUID postSettlement(
            UUID branchId,
            UUID fundingAccountId,
            BigDecimal amount,
            LocalDate settlementDate,
            UUID sourceId,
            String reference
    ) {

        UUID tenantId =
                TenantContext.getTenantId();

        UUID payableAccountId =
                accountingAccounts.get(
                        tenantId,
                        branchId,
                        "OPERATIONAL_EXPENSE_PAYABLE"
                );

        UUID eventId =
                UUID.nameUUIDFromBytes(
                        (
                                "OP_EXP_SETTLEMENT:"
                                        + sourceId
                        ).getBytes()
                );

        JournalEntry journal =
                accountingFacade.post(
                        AccountingEvent.builder()
                                .eventId(eventId)
                                .tenantId(tenantId)
                                .branchId(branchId)
                                .sourceModule("OPERATIONAL_EXPENSE_SETTLEMENT")
                                .sourceId(sourceId)
                                .reference(reference)
                                .description("Operational expense settlement")
                                .performedBy("SYSTEM")
                                .accountingDate(settlementDate)
                                .entries(
                                        List.of(
                                                AccountingEvent.Entry.builder()
                                                        .accountId(payableAccountId)
                                                        .direction(EntryDirection.DEBIT)
                                                        .amount(amount)
                                                        .build(),

                                                AccountingEvent.Entry.builder()
                                                        .accountId(fundingAccountId)
                                                        .direction(EntryDirection.CREDIT)
                                                        .amount(amount)
                                                        .build()
                                        )
                                )
                                .build()
                );

        return journal.getId();
    }
}