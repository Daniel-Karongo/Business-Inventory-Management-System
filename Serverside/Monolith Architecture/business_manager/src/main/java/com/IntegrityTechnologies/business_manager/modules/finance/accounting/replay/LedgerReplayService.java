package com.IntegrityTechnologies.business_manager.modules.finance.accounting.replay;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.*;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

@Service
@RequiredArgsConstructor
public class LedgerReplayService {

    private final LedgerEntryRepository ledgerRepo;
    private final AccountBalanceRepository balanceRepo;

    private static final int BATCH_SIZE = 2000;
    private final Set<UUID> activeReplays = ConcurrentHashMap.newKeySet();

    public void rebuildBranch(UUID branchId) {

        if (!activeReplays.add(branchId))
            throw new IllegalStateException("Replay already running for branch");

        try {

            balanceRepo.deleteBranchBalances(branchId);

            int page = 0;

            while (true) {

                List<LedgerEntry> batch =
                        ledgerRepo.streamBranchLedger(
                                branchId,
                                PageRequest.of(page, BATCH_SIZE)
                        );

                if (batch.isEmpty())
                    break;

                applyBatch(branchId, batch);

                page++;
            }

        } finally {
            activeReplays.remove(branchId);
        }
    }

    @Transactional
    protected void applyBatch(UUID branchId, List<LedgerEntry> batch) {

        for (LedgerEntry entry : batch) {

            BigDecimal delta =
                    entry.getDirection() == EntryDirection.DEBIT
                            ? entry.getAmount()
                            : entry.getAmount().negate();

            balanceRepo.applyDelta(
                    entry.getAccount().getId(),
                    branchId,
                    delta
            );
        }
    }
}