package com.IntegrityTechnologies.business_manager.modules.finance.accounting.policy;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.List;

@Component
public class DoubleEntryPolicy implements AccountingPolicy {

    @Override
    public void validate(List<LedgerEntry> entries) {

        if (entries == null || entries.size() < 2) {
            throw new IllegalStateException(
                    "Double-entry journal must contain at least 2 ledger lines"
            );
        }

        BigDecimal debit = BigDecimal.ZERO;
        BigDecimal credit = BigDecimal.ZERO;

        for (LedgerEntry e : entries) {

            if (e.getAmount() == null || e.getAmount().signum() <= 0) {
                throw new IllegalStateException("Ledger entry amount must be positive");
            }

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