package com.IntegrityTechnologies.business_manager.modules.finance.accounting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.ManualJournalRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class ManualJournalService {

    private final AccountingFacade accountingFacade;
    private final AccountRepository accountRepository;

    @Transactional
    public void post(ManualJournalRequest request) {

        if (request.lines() == null || request.lines().size() < 2) {
            throw new IllegalArgumentException("At least two journal lines are required");
        }

        request.lines().forEach(l -> {
            Account account = accountRepository.findById(l.accountId())
                    .orElseThrow(() -> new IllegalArgumentException("Account not found"));

            if (!account.isActive()) {
                throw new IllegalStateException("Inactive account: " + account.getCode());
            }

            if (l.amount() == null || l.amount().signum() <= 0) {
                throw new IllegalArgumentException("Amount must be greater than zero");
            }
        });

        accountingFacade.post(
                AccountingEvent.builder()
                        .sourceModule("MANUAL_JOURNAL")
                        .sourceId(UUID.randomUUID())
                        .reference(request.reference())
                        .description(request.description())
                        .performedBy(SecurityUtils.currentUsername())
                        .entries(
                                request.lines().stream()
                                        .map(l -> AccountingEvent.Entry.builder()
                                                .accountId(l.accountId())
                                                .direction(l.direction())
                                                .amount(l.amount())
                                                .build())
                                        .toList()
                        )
                        .build()
        );
    }
}