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

@Service
@RequiredArgsConstructor
public class DashboardSnapshotService {

    private final LedgerEntryRepository ledgerRepo;
    private final DashboardDailySnapshotRepository snapshotRepo;
    private final AccountingAccounts accounts;
    private final AccountBalanceRepository balanceRepo;
    private final InventoryValuationService valuationService;

    @Transactional
    public void compute(LocalDate date) {

        snapshotRepo.findByDate(date).ifPresent(s -> {
            return;
        });

        LocalDateTime start = date.atStartOfDay();
        LocalDateTime end = date.atTime(23,59,59);

        BigDecimal revenue =
                ledgerRepo.netMovementForAccount(
                        accounts.revenue(),
                        start,
                        end,
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
                        DEBIT_NORMAL,
                        CREDIT_NORMAL,
                        DEBIT,
                        CREDIT
                );

        BigDecimal vat =
                balanceRepo.findByAccount_Id(accounts.vatPayable())
                        .map(b -> b.getBalance())
                        .orElse(BigDecimal.ZERO);

        BigDecimal cash =
                balanceRepo.findByAccount_Id(accounts.cash())
                        .map(b -> b.getBalance())
                        .orElse(BigDecimal.ZERO);

        BigDecimal inventory =
                (BigDecimal) valuationService.getTotalValuation()
                        .get("totalValuation");

        DashboardDailySnapshot snap = new DashboardDailySnapshot();
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

    public void backfillMissingSnapshots(LocalDate from, LocalDate to) {

        Set<LocalDate> existing =
                new HashSet<>(snapshotRepo.findExistingDatesBetween(from, to));

        LocalDate date = from;

        while (!date.isAfter(to)) {

            if (!existing.contains(date)) {
                compute(date);
            }

            date = date.plusDays(1);
        }
    }
}