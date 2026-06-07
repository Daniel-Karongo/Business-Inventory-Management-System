package com.IntegrityTechnologies.business_manager.modules.finance.accounting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountBalance;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class AutoFundingService {

    private final AccountingFacade accountingFacade;
    private final AccountingAccounts accountingAccounts;
    private final AccountBalanceRepository accountBalanceRepository;

    @Transactional
    public BigDecimal ensureFundingAvailable(
            UUID branchId,
            UUID fundingAccountId,
            BigDecimal requiredAmount,
            LocalDate accountingDate,
            String reference
    ) {

        if (
                requiredAmount == null
                        || requiredAmount.compareTo(BigDecimal.ZERO) <= 0
        ) {
            return BigDecimal.ZERO;
        }

        UUID tenantId =
                TenantContext.getTenantId();

        UUID sourceId =
                UUID.nameUUIDFromBytes(
                        (
                                "AUTO_FUNDING:" +
                                        branchId +
                                        ":" +
                                        fundingAccountId +
                                        ":" +
                                        reference
                        ).getBytes()
                );

        if (
                accountingFacade.isAlreadyPosted(
                        "AUTO_FUNDING",
                        sourceId
                )
        ) {
            return BigDecimal.ZERO;
        }

        BigDecimal currentBalance =
                accountBalanceRepository
                        .findByTenantIdAndAccount_IdAndBranchId(
                                tenantId,
                                fundingAccountId,
                                branchId
                        )
                        .map(AccountBalance::getBalance)
                        .orElse(BigDecimal.ZERO);

        if (
                currentBalance.compareTo(requiredAmount) >= 0
        ) {
            return BigDecimal.ZERO;
        }

        BigDecimal shortfall =
                requiredAmount.subtract(currentBalance);

        UUID equityAccountId =
                accountingAccounts.get(
                        tenantId,
                        branchId,
                        "EQUITY"
                );

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(
                                UUID.nameUUIDFromBytes(
                                        (
                                                "AUTO_FUNDING_EVENT:" +
                                                        branchId +
                                                        ":" +
                                                        fundingAccountId +
                                                        ":" +
                                                        reference
                                        ).getBytes()
                                )
                        )
                        .tenantId(
                                tenantId
                        )
                        .branchId(
                                branchId
                        )
                        .sourceModule(
                                "AUTO_FUNDING"
                        )
                        .sourceId(
                                sourceId
                        )
                        .reference(
                                "AUTO-FUND-" + fundingAccountId
                        )
                        .description(
                                "Automatic funding from owner equity"
                        )
                        .performedBy(
                                currentUser()
                        )
                        .accountingDate(
                                accountingDate
                        )
                        .entries(
                                List.of(
                                        AccountingEvent.Entry.builder()
                                                .accountId(
                                                        fundingAccountId
                                                )
                                                .direction(
                                                        EntryDirection.DEBIT
                                                )
                                                .amount(
                                                        shortfall
                                                )
                                                .build(),

                                        AccountingEvent.Entry.builder()
                                                .accountId(
                                                        equityAccountId
                                                )
                                                .direction(
                                                        EntryDirection.CREDIT
                                                )
                                                .amount(
                                                        shortfall
                                                )
                                                .build()
                                )
                        )
                        .build()
        );

        return shortfall;
    }

    private String currentUser() {
        var auth =
                SecurityContextHolder
                        .getContext()
                        .getAuthentication();

        return auth != null
                ? auth.getName()
                : "SYSTEM";
    }
}