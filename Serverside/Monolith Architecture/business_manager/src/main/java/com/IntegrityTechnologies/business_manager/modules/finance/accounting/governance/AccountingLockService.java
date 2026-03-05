package com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.JournalEntryRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class AccountingLockService {

    private final JournalEntryRepository journalRepo;

    public void ensureNoJournalsExist(UUID branchId) {

        if (branchId == null) {
            throw new IllegalStateException("BranchId required");
        }

        if (journalRepo.countByBranch_Id(branchId) > 0) {
            throw new IllegalStateException(
                    "Accounting mode cannot be changed after journals exist for this branch."
            );
        }
    }
}