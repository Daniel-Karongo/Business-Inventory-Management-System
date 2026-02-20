package com.IntegrityTechnologies.business_manager.modules.finance.accounting.api;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller.JournalController;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.policy.AccountingPolicy;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.engine.LedgerPostingService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountingPeriodRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.JournalEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.TaxPeriodRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class AccountingFacade {

    private final AccountRepository accountRepository;
    private final LedgerPostingService postingService;
    private final JournalEntryRepository journalRepo;
    private final AccountingPolicy accountingPolicy;
    private final TaxPeriodRepository taxPeriodRepository;
    private final AccountingPeriodRepository accountingPeriodRepository;

    @Transactional
    public void post(AccountingEvent event) {

        // --------------------------------------
        // PERIOD LOCK CHECK
        // --------------------------------------
        LocalDate today = LocalDate.now();

        accountingPeriodRepository
                .findByStartDateLessThanEqualAndEndDateGreaterThanEqual(today, today)
                .filter(AccountingPeriod::isClosed)
                .ifPresent(p -> {
                    throw new IllegalStateException(
                            "Cannot post journal. Accounting period is closed."
                    );
                });

        JournalEntry journal = new JournalEntry();

        journal.setReference(event.getReference());
        journal.setSourceModule(event.getSourceModule());
        journal.setSourceId(event.getSourceId());
        journal.setDescription(event.getDescription());
        journal.setPostedBy(event.getPerformedBy());

        List<LedgerEntry> ledger = new ArrayList<>();

        for (var e : event.getEntries()) {
            Account account = accountRepository.findById(e.getAccountId())
                    .orElseThrow(() -> new IllegalStateException("Account not found"));

            ledger.add(new LedgerEntry(
                    account,
                    journal,
                    e.getDirection(),
                    e.getAmount()
            ));
        }

        postingService.post(journal, ledger);
    }

    @Transactional
    public void reverseJournal(UUID journalId, String reason, String user) {

        JournalEntry original = journalRepo.findById(journalId)
                .orElseThrow(() -> new IllegalArgumentException("Journal not found"));

        if (original.isReversed()) {
            throw new IllegalStateException("Journal already reversed");
        }

        if ("JOURNAL_REVERSAL".equals(original.getSourceModule())) {
            throw new IllegalStateException("Cannot reverse a reversal journal");
        }

        JournalEntry reversal = new JournalEntry();
        reversal.setReference("REV-" + original.getReference());
        reversal.setSourceModule("JOURNAL_REVERSAL");
        reversal.setDescription("Reversal: " + reason);
        reversal.markPosted(user);

        List<LedgerEntry> reversed =
                accountingPolicy.reverse(original, reversal);

        original.markReversed(reversal.getId());

        postingService.post(reversal, reversed);
    }

    @Transactional(readOnly = true)
    public boolean isAlreadyPosted(String sourceModule, UUID sourceId) {
        return journalRepo.existsBySourceModuleAndSourceId(sourceModule, sourceId);
    }
}