package com.IntegrityTechnologies.business_manager.modules.finance.accounting.policy;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;

import java.util.List;

public interface AccountingPolicy {

    /**
     * Validate ledger entries according to accounting rules.
     * - Double-entry: total debit == total credit
     * - Single-entry: only one side allowed
     */
    void validate(List<LedgerEntry> entries);

    List<LedgerEntry> reverse(
            JournalEntry original,
            JournalEntry reversal
    );
}