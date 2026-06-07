package com.IntegrityTechnologies.business_manager.modules.finance.accounting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
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

    @Transactional
    public void postExpense(
            UUID branchId,
            UUID fundingAccountId,
            UUID expenseAccountId,
            String reference,
            String description,
            BigDecimal amount,
            LocalDate accountingDate,
            int expenseIndex
    ) {

        if (
                amount == null
                        || amount.compareTo(BigDecimal.ZERO) <= 0
        ) {
            return;
        }

        UUID tenantId =
                TenantContext.getTenantId();

        UUID sourceId =
                UUID.nameUUIDFromBytes(
                        (
                                "STOCK_EXPENSE:" +
                                        branchId +
                                        ":" +
                                        reference +
                                        ":" +
                                        expenseIndex
                        ).getBytes()
                );

        if (
                accountingFacade.isAlreadyPosted(
                        "STOCK_ONBOARDING_EXPENSE",
                        sourceId
                )
        ) {
            return;
        }

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(
                                UUID.nameUUIDFromBytes(
                                        (
                                                "STOCK_EXPENSE_EVENT:" +
                                                        branchId +
                                                        ":" +
                                                        reference +
                                                        ":" +
                                                        expenseIndex
                                        ).getBytes()
                                )
                        )
                        .tenantId(tenantId)
                        .branchId(branchId)
                        .sourceModule("STOCK_ONBOARDING_EXPENSE")
                        .sourceId(sourceId)
                        .reference(reference)
                        .description(description)
                        .performedBy("SYSTEM")
                        .accountingDate(accountingDate)
                        .entries(
                                List.of(
                                        AccountingEvent.Entry.builder()
                                                .accountId(
                                                        expenseAccountId
                                                )
                                                .direction(
                                                        EntryDirection.DEBIT
                                                )
                                                .amount(amount)
                                                .build(),

                                        AccountingEvent.Entry.builder()
                                                .accountId(
                                                        fundingAccountId
                                                )
                                                .direction(
                                                        EntryDirection.CREDIT
                                                )
                                                .amount(amount)
                                                .build()
                                )
                        )
                        .build()
        );
    }
}