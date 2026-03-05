package com.IntegrityTechnologies.business_manager.modules.dashboard.service;

import com.IntegrityTechnologies.business_manager.modules.dashboard.model.DashboardDailySnapshot;
import com.IntegrityTechnologies.business_manager.modules.dashboard.repository.DashboardDailySnapshotRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryValuationService;
import static com.IntegrityTechnologies.business_manager.modules.finance.accounting.support.AccountingSignRules.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.*;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class DashboardSnapshotService {

    private final LedgerEntryRepository ledgerRepo;
    private final DashboardDailySnapshotRepository snapshotRepo;
    private final AccountingAccounts accounts;
    private final AccountBalanceRepository balanceRepo;
    private final InventoryValuationService valuationService;

    @Transactional
    public void compute(UUID branchId, LocalDate date) {

        snapshotRepo.findByBranchIdAndDate(branchId, date).ifPresent(s -> {
            return;
        });

        LocalDateTime start = date.atStartOfDay();
        LocalDateTime end = date.atTime(23,59,59);

        BigDecimal revenue =
                ledgerRepo.netMovementForAccount(
                        accounts.revenue(),
                        start,
                        end,
                        branchId,
                        DEBIT_NORMAL,
                        CREDIT_NORMAL,
                        DEBIT,
                        CREDIT
                );

        BigDecimal cogs =
                ledgerRepo.netMovementForAccount(
                        accounts.cogs(),
                        start,
                        end,
                        branchId,
                        DEBIT_NORMAL,
                        CREDIT_NORMAL,
                        DEBIT,
                        CREDIT
                );

        BigDecimal vat =
                balanceRepo.findByAccount_IdAndBranch_Id(
                                accounts.vatPayable(),
                                branchId
                        )
                        .map(b -> b.getBalance())
                        .orElse(BigDecimal.ZERO);

        BigDecimal cash =
                balanceRepo.findByAccount_IdAndBranch_Id(accounts.cash(), branchId)
                        .map(b -> b.getBalance())
                        .orElse(BigDecimal.ZERO)
                        .add(
                                balanceRepo.findByAccount_IdAndBranch_Id(accounts.bank(), branchId)
                                        .map(b -> b.getBalance())
                                        .orElse(BigDecimal.ZERO)
                        ).add(
                                balanceRepo.findByAccount_IdAndBranch_Id(accounts.mpesa(), branchId)
                                        .map(b -> b.getBalance())
                                        .orElse(BigDecimal.ZERO)
                        );

        BigDecimal inventory =
                (BigDecimal) valuationService.getBranchValuation(branchId)
                        .get("totalValuation");

        DashboardDailySnapshot snap = new DashboardDailySnapshot();
        snap.setBranchId(branchId);
        snap.setDate(date);
        snap.setRevenue(revenue);
        snap.setCogs(cogs);
        snap.setProfit(revenue.subtract(cogs));
        snap.setVat(vat);
        snap.setCash(cash);
        snap.setInventory(inventory);
        snap.setComputedAt(LocalDateTime.now());

        try {
            snapshotRepo.save(snap);
        } catch (Exception ignored) {
            // another thread inserted it first — safe to ignore
        }
    }

    public void backfillMissingSnapshots(UUID branchId, LocalDate from, LocalDate to) {

        Set<LocalDate> existing =
                new HashSet<>(snapshotRepo.findExistingDatesBetween(branchId, from, to));

        LocalDate date = from;

        while (!date.isAfter(to)) {

            if (!existing.contains(date)) {
                compute(branchId, date);
            }

            date = date.plusDays(1);
        }
    }
}