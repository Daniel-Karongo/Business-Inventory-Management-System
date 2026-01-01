package com.IntegrityTechnologies.business_manager.modules.finance.accounting.policy;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import org.springframework.stereotype.Component;

import java.util.List;

public class SingleEntryPolicy implements AccountingPolicy {

    @Override
    public void validate(List<LedgerEntry> entries) {
        if (entries == null || entries.isEmpty()) {
            throw new IllegalStateException("At least one ledger entry is required");
        }
    }
}