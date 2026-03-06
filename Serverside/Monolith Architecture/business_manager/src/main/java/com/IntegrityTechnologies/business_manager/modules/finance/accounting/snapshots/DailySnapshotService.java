package com.IntegrityTechnologies.business_manager.modules.finance.accounting.snapshots;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

@Service
@RequiredArgsConstructor
public class DailySnapshotService {

    private final LedgerEntryRepository ledgerRepo;
    private final DailyAccountBalanceSnapshotRepository snapshotRepo;

    @Transactional
    public void snapshotBranch(UUID branchId) {

        LocalDate snapshotDate = LocalDate.now().minusDays(1);

        LocalDateTime start = snapshotDate.atStartOfDay();
        LocalDateTime end = snapshotDate.atTime(23,59,59);

        Map<UUID, BigDecimal> openingMap =
                buildOpeningBalances(branchId, start);

        Map<UUID, BigDecimal> debitMap = new HashMap<>();
        Map<UUID, BigDecimal> creditMap = new HashMap<>();

        List<Object[]> movement =
                ledgerRepo.movementByAccountBetween(
                        start,
                        end,
                        branchId,
                        EntryDirection.DEBIT,
                        EntryDirection.CREDIT
                );

        for (Object[] row : movement) {

            UUID accountId = (UUID) row[0];

            BigDecimal debit = safe((BigDecimal) row[1]);
            BigDecimal credit = safe((BigDecimal) row[2]);

            debitMap.put(accountId, debit);
            creditMap.put(accountId, credit);
        }

        Set<UUID> accounts = new HashSet<>();
        accounts.addAll(openingMap.keySet());
        accounts.addAll(debitMap.keySet());
        accounts.addAll(creditMap.keySet());

        List<DailyAccountBalanceSnapshot> batch = new ArrayList<>();

        for (UUID accountId : accounts) {

            BigDecimal opening =
                    openingMap.getOrDefault(accountId, BigDecimal.ZERO);

            BigDecimal debit =
                    debitMap.getOrDefault(accountId, BigDecimal.ZERO);

            BigDecimal credit =
                    creditMap.getOrDefault(accountId, BigDecimal.ZERO);

            BigDecimal closing =
                    opening.add(debit).subtract(credit);

            DailyAccountBalanceSnapshot snap =
                    new DailyAccountBalanceSnapshot();

            snap.setBranchId(branchId);
            snap.setAccountId(accountId);
            snap.setSnapshotDate(snapshotDate);

            snap.setOpeningBalance(opening);
            snap.setDebitTotal(debit);
            snap.setCreditTotal(credit);
            snap.setClosingBalance(closing);

            snap.setCreatedAt(LocalDateTime.now());

            batch.add(snap);
        }

        snapshotRepo.saveAll(batch);
    }

    private Map<UUID, BigDecimal> buildOpeningBalances(
            UUID branchId,
            LocalDateTime start
    ) {

        Map<UUID, BigDecimal> map = new HashMap<>();

        List<Object[]> rows =
                ledgerRepo.balanceBeforeDate(
                        start,
                        branchId,
                        EntryDirection.DEBIT,
                        EntryDirection.CREDIT
                );

        for (Object[] row : rows) {

            map.put(
                    (UUID) row[0],
                    safe((BigDecimal) row[1])
            );
        }

        return map;
    }

    private BigDecimal safe(BigDecimal val) {
        return val == null ? BigDecimal.ZERO : val;
    }
}