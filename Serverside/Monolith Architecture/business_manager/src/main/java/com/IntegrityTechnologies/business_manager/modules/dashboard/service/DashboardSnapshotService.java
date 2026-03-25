package com.IntegrityTechnologies.business_manager.modules.dashboard.service;

import com.IntegrityTechnologies.business_manager.modules.dashboard.model.DashboardDailySnapshot;
import com.IntegrityTechnologies.business_manager.modules.dashboard.repository.DashboardDailySnapshotRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.valuation.InventoryValuationService;

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

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Transactional
    public void compute(UUID branchId, LocalDate date) {

        UUID tenantId = tenantId();

        if (snapshotRepo.findByTenantIdAndBranchIdAndDate(tenantId, branchId, date).isPresent()) {
            return;
        }

        LocalDateTime start = date.atStartOfDay();
        LocalDateTime end = date.atTime(23, 59, 59);

        BigDecimal revenue =
                ledgerRepo.netMovementForAccount(
                        tenantId,
                        accounts.get(tenantId, branchId, AccountRole.REVENUE),
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
                        tenantId,
                        accounts.get(tenantId, branchId, AccountRole.COGS),
                        start,
                        end,
                        branchId,
                        DEBIT_NORMAL,
                        CREDIT_NORMAL,
                        DEBIT,
                        CREDIT
                );

        BigDecimal vat =
                balanceRepo.findByTenantIdAndAccount_IdAndBranch_Id(
                                tenantId,
                                accounts.get(tenantId, branchId, AccountRole.VAT_PAYABLE),
                                branchId
                        )
                        .map(b -> b.getBalance())
                        .orElse(BigDecimal.ZERO);

        BigDecimal cash =
                balanceRepo.findByTenantIdAndAccount_IdAndBranch_Id(
                                tenantId,
                                accounts.get(tenantId, branchId, AccountRole.CASH),
                                branchId
                        )
                        .map(b -> b.getBalance())
                        .orElse(BigDecimal.ZERO)
                        .add(
                                balanceRepo.findByTenantIdAndAccount_IdAndBranch_Id(
                                                tenantId,
                                                accounts.get(tenantId, branchId, AccountRole.BANK),
                                                branchId
                                        )
                                        .map(b -> b.getBalance())
                                        .orElse(BigDecimal.ZERO)
                        )
                        .add(
                                balanceRepo.findByTenantIdAndAccount_IdAndBranch_Id(
                                                tenantId,
                                                accounts.get(tenantId, branchId, AccountRole.MPESA),
                                                branchId
                                        )
                                        .map(b -> b.getBalance())
                                        .orElse(BigDecimal.ZERO)
                        );

        BigDecimal inventory =
                (BigDecimal) valuationService.getBranchValuation(branchId)
                        .get("totalValuation");

        DashboardDailySnapshot snap = new DashboardDailySnapshot();
        snap.setTenantId(tenantId);
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
            // concurrent insert safe
        }
    }

    public void backfillMissingSnapshots(UUID branchId, LocalDate from, LocalDate to) {

        UUID tenantId = tenantId();

        Set<LocalDate> existing =
                new HashSet<>(snapshotRepo.findExistingDatesBetween(tenantId, branchId, from, to));

        LocalDate date = from;

        while (!date.isAfter(to)) {

            if (!existing.contains(date)) {
                compute(branchId, date);
            }

            date = date.plusDays(1);
        }
    }
}