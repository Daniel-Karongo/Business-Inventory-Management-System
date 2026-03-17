package com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.JournalEntryRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class AccountingLockService {

    private final JournalEntryRepository journalRepo;
    private final BranchTenantGuard branchTenantGuard;

    public void ensureNoJournalsExist(UUID branchId) {

        if (branchId == null) {
            throw new IllegalStateException("BranchId required");
        }

        branchTenantGuard.validate(branchId);

        UUID tenantId = TenantContext.getTenantId();

        if (journalRepo.countByTenantIdAndBranchId(tenantId, branchId) > 0) {

            throw new IllegalStateException(
                    "Accounting mode cannot be changed after journals exist for this branch."
            );
        }
    }
}