package com.IntegrityTechnologies.business_manager.modules.finance.accounting.policy;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.List;

public class DoubleEntryPolicy implements AccountingPolicy {

    @Override
    public void validate(List<LedgerEntry> entries) {
        BigDecimal debit = BigDecimal.ZERO;
        BigDecimal credit = BigDecimal.ZERO;

        for (LedgerEntry e : entries) {
            if (e.getDirection() == EntryDirection.DEBIT)
                debit = debit.add(e.getAmount());
            else
                credit = credit.add(e.getAmount());
        }

        if (debit.compareTo(credit) != 0) {
            throw new IllegalStateException(
                    "Unbalanced journal: debit=" + debit + " credit=" + credit
            );
        }
    }

    @Override
    public List<LedgerEntry> reverse(
            JournalEntry original,
            JournalEntry reversal
    ) {
        return original.getLedgerEntries().stream()
                .map(e -> new LedgerEntry(
                        e.getAccount(),
                        reversal,
                        e.getDirection() == EntryDirection.DEBIT
                                ? EntryDirection.CREDIT
                                : EntryDirection.DEBIT,
                        e.getAmount()
                ))
                .toList();
    }
}