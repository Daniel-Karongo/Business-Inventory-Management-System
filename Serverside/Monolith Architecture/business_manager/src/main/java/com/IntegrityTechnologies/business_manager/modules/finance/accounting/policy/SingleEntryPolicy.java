package com.IntegrityTechnologies.business_manager.modules.finance.accounting.policy;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import org.springframework.stereotype.Component;

import java.util.List;

public class SingleEntryPolicy implements AccountingPolicy {

    @Override
    public void validate(List<LedgerEntry> entries) {
        if (entries == null || entries.isEmpty()) {
            throw new IllegalStateException("At least one ledger entry is required");
        }
    }

    @Override
    public List<LedgerEntry> reverse(
            JournalEntry original,
            JournalEntry reversal
    ) {
        LedgerEntry originalEntry = original.getLedgerEntries().get(0);

        return List.of(
                new LedgerEntry(
                        originalEntry.getAccount(),
                        reversal,
                        originalEntry.getDirection() == EntryDirection.DEBIT
                                ? EntryDirection.CREDIT
                                : EntryDirection.DEBIT,
                        originalEntry.getAmount()
                )
        );
    }
}