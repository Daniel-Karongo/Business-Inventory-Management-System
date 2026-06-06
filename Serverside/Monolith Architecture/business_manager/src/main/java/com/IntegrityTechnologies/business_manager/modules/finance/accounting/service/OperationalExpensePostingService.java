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
            LocalDate accountingDate
    ) {

        if (
                amount == null
                        || amount.compareTo(BigDecimal.ZERO) <= 0
        ) {
            return;
        }

        UUID tenantId =
                TenantContext.getTenantId();

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(UUID.randomUUID())
                        .tenantId(tenantId)
                        .branchId(branchId)
                        .sourceModule("STOCK_ONBOARDING_EXPENSE")
                        .sourceId(UUID.randomUUID())
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