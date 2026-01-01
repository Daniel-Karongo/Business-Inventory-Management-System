package com.IntegrityTechnologies.business_manager.modules.finance.accounting.api;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.engine.LedgerPostingService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

@Service
@RequiredArgsConstructor
public class AccountingFacade {

    private final AccountRepository accountRepository;
    private final LedgerPostingService postingService;

    @Transactional
    public void post(AccountingEvent event) {

        JournalEntry journal = new JournalEntry();

        journal.setReference(event.getReference());
        journal.setSourceModule(event.getSourceModule());
        journal.setSourceId(event.getSourceId());
        journal.setDescription(event.getDescription());
        journal.markPosted(event.getPerformedBy());

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
}