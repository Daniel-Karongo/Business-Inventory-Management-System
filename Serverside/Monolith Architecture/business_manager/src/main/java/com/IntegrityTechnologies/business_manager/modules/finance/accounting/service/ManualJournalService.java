package com.IntegrityTechnologies.business_manager.modules.finance.accounting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountingMode;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.ManualJournalRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.AccountingSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class ManualJournalService {

    private final AccountingFacade accountingFacade;
    private final AccountRepository accountRepository;
    private final PeriodGuardService periodGuardService;

    @Transactional
    public void post(ManualJournalRequest request) {

        if (request.branchId() == null)
            throw new IllegalArgumentException("BranchId is required.");

        if (request.accountingDate() == null)
            throw new IllegalArgumentException("Accounting date is required.");

        if (request.lines() == null || request.lines().isEmpty())
            throw new IllegalArgumentException("At least one journal line is required.");

        periodGuardService.validateOpenPeriod(
                request.accountingDate(),
                request.branchId()
        );

        accountingFacade.post(
                AccountingEvent.builder()
                        .eventId(UUID.randomUUID())
                        .sourceModule("MANUAL_JOURNAL")
                        .sourceId(UUID.randomUUID())
                        .reference(request.reference())
                        .description(request.description())
                        .performedBy(SecurityUtils.currentUsername())
                        .branchId(request.branchId())
                        .accountingDate(request.accountingDate())
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