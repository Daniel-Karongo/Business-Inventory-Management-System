package com.IntegrityTechnologies.business_manager.modules.finance.accounting.engine;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.policy.AccountingPolicy;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.JournalEntryRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@RequiredArgsConstructor
public class LedgerPostingService {

    private final AccountingPolicy policy;
    private final JournalEntryRepository journalRepo;

    @Transactional
    public JournalEntry post(JournalEntry journal, List<LedgerEntry> entries) {

        policy.validate(entries);

        journal.markPosted(journal.getPostedBy());
        journal.getLedgerEntries().addAll(entries);

        return journalRepo.save(journal);
    }
}
