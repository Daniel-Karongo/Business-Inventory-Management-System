package com.IntegrityTechnologies.business_manager.modules.finance.accounting.replay;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.IntegrityTechnologies.business_manager.security.BranchTenantGuard;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

@Service
@RequiredArgsConstructor
public class LedgerReplayService {

    private final LedgerEntryRepository ledgerRepo;
    private final AccountBalanceRepository balanceRepo;
    private final BranchTenantGuard branchTenantGuard;

    private static final int BATCH_SIZE = 2000;

    private final Set<UUID> activeReplays = ConcurrentHashMap.newKeySet();

    public void rebuildBranch(UUID branchId) {

        branchTenantGuard.validate(branchId);

        UUID tenantId = TenantContext.getTenantId();

        if (!activeReplays.add(branchId))
            throw new IllegalStateException("Replay already running for branch");

        try {

            balanceRepo.deleteBranchBalances(
                    tenantId,
                    branchId
            );

            int page = 0;

            while (true) {

                List<LedgerEntry> batch =
                        ledgerRepo.streamBranchLedger(
                                tenantId,
                                branchId,
                                PageRequest.of(page, BATCH_SIZE)
                        );

                if (batch.isEmpty())
                    break;

                applyBatch(
                        tenantId,
                        branchId,
                        batch
                );

                page++;
            }

        } finally {
            activeReplays.remove(branchId);
        }
    }

    @Transactional
    public void applyBatch(
            UUID tenantId,
            UUID branchId,
            List<LedgerEntry> batch
    ) {

        for (LedgerEntry entry : batch) {

            BigDecimal delta =
                    entry.getDirection() == EntryDirection.DEBIT
                            ? entry.getAmount()
                            : entry.getAmount().negate();

            balanceRepo.applyDelta(
                    tenantId,
                    entry.getAccount().getId(),
                    branchId,
                    delta
            );
        }
    }
}