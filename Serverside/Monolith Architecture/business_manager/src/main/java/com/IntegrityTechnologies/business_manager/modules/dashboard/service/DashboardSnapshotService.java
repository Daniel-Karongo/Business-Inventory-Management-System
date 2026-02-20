package com.IntegrityTechnologies.business_manager.modules.dashboard.service;

import com.IntegrityTechnologies.business_manager.modules.dashboard.model.DashboardDailySnapshot;
import com.IntegrityTechnologies.business_manager.modules.dashboard.repository.DashboardDailySnapshotRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryValuationService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.*;

@Service
@RequiredArgsConstructor
public class DashboardSnapshotService {

    private final LedgerEntryRepository ledgerRepo;
    private final DashboardDailySnapshotRepository snapshotRepo;
    private final AccountingAccounts accounts;
    private final AccountBalanceRepository balanceRepo;
    private final InventoryValuationService valuationService;

    public void compute(LocalDate date) {

        if (snapshotRepo.findByDate(date).isPresent()) return;

        LocalDateTime start = date.atStartOfDay();
        LocalDateTime end = date.atTime(23,59,59);

        BigDecimal revenue =
                ledgerRepo.netMovementForAccount(
                        accounts.revenue(), start, end
                ).abs();

        BigDecimal cogs =
                ledgerRepo.netMovementForAccount(
                        accounts.cogs(), start, end
                ).abs();

        BigDecimal vat =
                ledgerRepo.netMovementForAccount(
                        accounts.vatPayable(),
                        LocalDateTime.of(2000,1,1,0,0),
                        end
                );

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

        snapshotRepo.save(snap);
    }
}