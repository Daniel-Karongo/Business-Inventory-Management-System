package com.IntegrityTechnologies.business_manager.modules.finance.accounting.snapshots;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountBalance;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class DailySnapshotService {

    private final AccountBalanceRepository balanceRepo;
    private final DailyAccountBalanceSnapshotRepository snapshotRepo;

    @Transactional
    public void snapshotBranch(UUID branchId) {

        int page = 0;
        int size = 2000;

        Page<AccountBalance> result;

        do {

            result =
                    balanceRepo.findByBranch_Id(
                            branchId,
                            PageRequest.of(page++, size)
                    );

            List<DailyAccountBalanceSnapshot> batch = new ArrayList<>();

            for (AccountBalance balance : result.getContent()) {

                DailyAccountBalanceSnapshot snapshot =
                        new DailyAccountBalanceSnapshot();

                snapshot.setBranchId(branchId);
                snapshot.setAccountId(balance.getAccount().getId());
                snapshot.setSnapshotDate(LocalDate.now());
                snapshot.setBalance(balance.getBalance());
                snapshot.setCreatedAt(LocalDateTime.now());

                batch.add(snapshot);
            }

            snapshotRepo.saveAll(batch);

        } while (result.hasNext());
    }

    public void backfillSnapshots(UUID branchId) {

        LocalDate lastSnapshot =
                snapshotRepo
                        .findTopByBranchIdOrderBySnapshotDateDesc(branchId)
                        .map(DailyAccountBalanceSnapshot::getSnapshotDate)
                        .orElse(LocalDate.now().minusDays(1));

        LocalDate today = LocalDate.now();

        while (lastSnapshot.isBefore(today)) {

            lastSnapshot = lastSnapshot.plusDays(1);

            snapshotBranchForDate(branchId, lastSnapshot);
        }
    }

    @Transactional
    public void snapshotBranchForDate(UUID branchId, LocalDate date) {

        int page = 0;
        int size = 2000;

        Page<AccountBalance> result;

        do {

            result =
                    balanceRepo.findByBranch_Id(
                            branchId,
                            PageRequest.of(page++, size)
                    );

            List<DailyAccountBalanceSnapshot> batch = new ArrayList<>();

            for (AccountBalance balance : result.getContent()) {

                DailyAccountBalanceSnapshot snapshot =
                        new DailyAccountBalanceSnapshot();

                snapshot.setBranchId(branchId);
                snapshot.setAccountId(balance.getAccount().getId());
                snapshot.setSnapshotDate(date);
                snapshot.setBalance(balance.getBalance());
                snapshot.setCreatedAt(LocalDateTime.now());

                batch.add(snapshot);
            }

            snapshotRepo.saveAll(batch);

        } while (result.hasNext());
    }
}