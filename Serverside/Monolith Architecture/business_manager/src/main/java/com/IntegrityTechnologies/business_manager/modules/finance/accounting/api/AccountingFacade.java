package com.IntegrityTechnologies.business_manager.modules.finance.accounting.api;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.cache.AccountingLedgerUpdatedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountingMode;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.engine.LedgerPostingService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.AccountingSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.policy.AccountingPolicy;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.JournalEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.PeriodGuardService;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class AccountingFacade {

    private final AccountRepository accountRepository;
    private final LedgerPostingService postingService;
    private final JournalEntryRepository journalRepo;
    private final PeriodGuardService periodGuardService;
    private final Map<AccountingMode, AccountingPolicy> policies;
    private final AccountingSystemStateService stateService;
    private final ApplicationEventPublisher applicationEventPublisher;

    @Transactional
    public void post(AccountingEvent event) {

        UUID tenantId = TenantContext.getTenantId();

        if (event.getEventId() == null)
            throw new IllegalArgumentException("EventId required");

        if (journalRepo.existsByTenantIdAndAccountingEventId(
                tenantId,
                event.getEventId()
        )) return;

        if (event.getBranchId() == null)
            throw new IllegalStateException("BranchId required");

        AccountingMode mode = stateService.getMode(event.getBranchId());
        AccountingPolicy policy = policies.get(mode);

        LocalDate accountingDate =
                event.getAccountingDate() != null
                        ? event.getAccountingDate()
                        : LocalDate.now();

        AccountingPeriod period =
                periodGuardService.validateOpenPeriod(
                        accountingDate,
                        event.getBranchId()
                );

        JournalEntry journal = new JournalEntry(
                tenantId,
                event.getBranchId(),
                event.getReference(),
                event.getSourceModule(),
                event.getSourceId(),
                event.getDescription(),
                accountingDate,
                event.getEventId(),
                period
        );

        List<LedgerEntry> ledger = new ArrayList<>();

        for (var e : event.getEntries()) {

            Account account =
                    accountRepository
                            .findByTenantIdAndBranchIdAndId(
                                    tenantId,
                                    event.getBranchId(),
                                    e.getAccountId()
                            )
                            .orElseThrow(() ->
                                    new IllegalStateException(
                                            "Account not found in tenant and branch"
                                    ));

            ledger.add(new LedgerEntry(
                    tenantId,
                    event.getBranchId(),
                    account,
                    journal,
                    e.getDirection(),
                    e.getAmount()
            ));
        }

        policy.validate(ledger);

        postingService.post(journal, ledger, event.getPerformedBy());

        applicationEventPublisher.publishEvent(
                new AccountingLedgerUpdatedEvent(
                        tenantId,
                        event.getBranchId()
                )
        );

        stateService.lockIfNecessary(event.getBranchId());
    }

    @Transactional
    public void reverseJournal(UUID journalId, String reason, String user) {

        UUID tenantId = TenantContext.getTenantId();

        JournalEntry original =
                journalRepo
                        .findByTenantIdAndId(tenantId, journalId)
                        .orElseThrow(() ->
                                new IllegalArgumentException("Journal not found"));

        if (original.isReversed())
            throw new IllegalStateException("Journal already reversed");

        UUID branchId = original.getBranch().getId();

        periodGuardService.validateOpenPeriod(
                original.getAccountingDate(),
                branchId
        );

        LocalDate reversalDate = LocalDate.now();

        AccountingPeriod reversalPeriod =
                periodGuardService.validateOpenPeriod(
                        reversalDate,
                        branchId
                );

        UUID reversalEventId = UUID.randomUUID();

        JournalEntry reversal = new JournalEntry(
                tenantId,
                branchId,
                "REV-" + original.getReference(),
                "JOURNAL_REVERSAL",
                UUID.randomUUID(),
                "Reversal: " + reason,
                reversalDate,
                reversalEventId,
                reversalPeriod
        );

        AccountingMode mode = stateService.getMode(branchId);
        AccountingPolicy policy = policies.get(mode);

        List<LedgerEntry> reversed =
                policy.reverse(original, reversal);

        JournalEntry postedReversal =
                postingService.post(reversal, reversed, user);

        original.markReversed(postedReversal.getId());

        journalRepo.save(original);

        applicationEventPublisher.publishEvent(
                new AccountingLedgerUpdatedEvent(
                        tenantId,
                        branchId
                )
        );
    }

    @Transactional(readOnly = true)
    public boolean isAlreadyPosted(String sourceModule, UUID sourceId) {

        UUID tenantId = TenantContext.getTenantId();

        return journalRepo.existsByTenantIdAndSourceModuleAndSourceId(
                tenantId,
                sourceModule,
                sourceId
        );
    }
}