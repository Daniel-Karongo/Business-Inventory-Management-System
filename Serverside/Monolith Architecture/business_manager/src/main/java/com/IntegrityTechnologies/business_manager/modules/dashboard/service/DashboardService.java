package com.IntegrityTechnologies.business_manager.modules.dashboard.service;

import com.IntegrityTechnologies.business_manager.modules.dashboard.dto.*;
import com.IntegrityTechnologies.business_manager.modules.dashboard.repository.DashboardDailySnapshotRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.budget.domain.Budget;
import com.IntegrityTechnologies.business_manager.modules.finance.budget.repository.BudgetRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.config.TaxProperties;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.BatchConsumptionRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryItemRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryValuationService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.*;
import java.util.*;

import static com.IntegrityTechnologies.business_manager.modules.finance.accounting.support.AccountingSignRules.*;


@Service
@RequiredArgsConstructor
public class DashboardService {

    private final LedgerEntryRepository ledgerRepo;
    private final AccountingAccounts accounts;
    private final AccountBalanceRepository balanceRepo;
    private final InventoryValuationService valuationService;
    private final InventoryItemRepository inventoryItemRepository;
    private final TaxProperties taxProperties;
    private final SaleRepository saleRepository;
    private final BatchConsumptionRepository batchConsumptionRepository;
    private final BudgetRepository budgetRepository;
    private final DashboardDailySnapshotRepository snapshotRepo;
    private final DashboardSnapshotService snapshotService;

    /* ============================================================
       MAIN ENTRY
    ============================================================ */
    public DashboardSummaryDTO getBranchDashboard(UUID branchId) {

        LocalDate today = LocalDate.now();
        LocalDateTime start = today.atStartOfDay();
        LocalDateTime end = today.atTime(23,59,59);

        Map<String, List<ChartPoint>> trends = build7DayTrends();

        return DashboardSummaryDTO.builder()
                .branchId(branchId)
                .date(today)
                .financial(buildFinancialKpis(start, end))
                .operational(buildOperationalKpis(branchId))
                .revenueTrend(trends.get("revenue"))
                .profitTrend(trends.get("profit"))
                .vatTrend(trends.get("vat"))
                .topBatches(top5ProfitableBatches())
                .recentActivities(List.of())
                .build();
    }

    /* ============================================================
       FINANCIAL KPIs (REAL-TIME BALANCE DRIVEN)
    ============================================================ */
    private DashboardSummaryDTO.FinancialKpis buildFinancialKpis(
            LocalDateTime start,
            LocalDateTime end
    ) {

        Set<UUID> kpiAccounts = Set.of(
                accounts.revenue(),
                accounts.cogs()
        );

        Map<UUID, BigDecimal> movements =
                fetchMovements(kpiAccounts, start, end);

        BigDecimal revenueToday = movements.get(accounts.revenue());
        BigDecimal cogsToday = movements.get(accounts.cogs());

        BigDecimal marginPercent =
                revenueToday.compareTo(BigDecimal.ZERO) == 0
                        ? BigDecimal.ZERO
                        : revenueToday.subtract(cogsToday)
                        .divide(revenueToday, 4, RoundingMode.HALF_UP)
                        .multiply(BigDecimal.valueOf(100));

        LocalDate today = LocalDate.now();
        LocalDate oneYearAgo = today.minusYears(1);

        snapshotService.backfillMissingSnapshots(oneYearAgo, today);

        BigDecimal todayCogs = movements.get(accounts.cogs());

        BigDecimal yearlyCogs =
                snapshotRepo.sumCogsBetween(oneYearAgo, today.minusDays(1))
                        .add(todayCogs);

        BigDecimal inventoryValue =
                (BigDecimal) valuationService.getTotalValuation()
                        .get("totalValuation");

        BigDecimal inventoryTurnover =
                inventoryValue.compareTo(BigDecimal.ZERO) == 0
                        ? BigDecimal.ZERO
                        : yearlyCogs.divide(inventoryValue, 4, RoundingMode.HALF_UP);

        BigDecimal expensesLast30 = ledgerRepo.totalExpensesBetween(
                LocalDate.now().minusDays(30).atStartOfDay(),
                LocalDateTime.now(),
                AccountType.EXPENSE,
                EntryDirection.DEBIT
        );

        BigDecimal burnRate =
                expensesLast30.divide(BigDecimal.valueOf(30), 2, RoundingMode.HALF_UP);

        BigDecimal vatPayable =
                balanceRepo.findByAccount_Id(accounts.vatPayable())
                        .map(b -> b.getBalance())
                        .orElse(BigDecimal.ZERO);

        BigDecimal ar =
                balanceRepo.findByAccount_Id(accounts.accountsReceivable())
                        .map(b -> b.getBalance())
                        .orElse(BigDecimal.ZERO);

        BigDecimal ap =
                balanceRepo.findByAccount_Id(accounts.accountsPayable())
                        .map(b -> b.getBalance())
                        .orElse(BigDecimal.ZERO);

        BigDecimal cash =
                balanceRepo.findByAccount_Id(accounts.cash())
                        .map(b -> b.getBalance())
                        .orElse(BigDecimal.ZERO)
                        .add(
                                balanceRepo.findByAccount_Id(accounts.bank())
                                        .map(b -> b.getBalance())
                                        .orElse(BigDecimal.ZERO)
                        );

        BigDecimal corporateTax =
                taxProperties.getBusinessTaxMode().name().equals("CORPORATE")
                        ? balanceRepo.findByAccount_Id(accounts.corporateTaxPayable())
                        .map(b -> b.getBalance())
                        .orElse(BigDecimal.ZERO)
                        : BigDecimal.ZERO;

        BigDecimal revenueBudgetVariance =
                computeMonthlyBudgetVariance(accounts.revenue());

        BigDecimal expenseBudgetVariance =
                computeMonthlyBudgetVariance(accounts.cogs());

        AgingBucketDTO aging = computeARAging();

        return DashboardSummaryDTO.FinancialKpis.builder()
                .netRevenueToday(revenueToday)
                .grossProfitToday(revenueToday.subtract(cogsToday))
                .grossMarginPercent(marginPercent)
                .inventoryTurnover(inventoryTurnover)
                .burnRate(burnRate)
                .vatPayable(vatPayable)
                .accountsReceivable(ar)
                .accountsPayable(ap)
                .cashBalance(cash)
                .inventoryValue(inventoryValue)
                .corporateTaxAccrued(corporateTax)
                .arAging(aging)
                .apAging(computeAPAging())
                .revenueBudgetVariance(revenueBudgetVariance)
                .expenseBudgetVariance(expenseBudgetVariance)
                .build();
    }

    /* ============================================================
       OPERATIONAL KPIs
    ============================================================ */
    private DashboardSummaryDTO.OperationalKpis buildOperationalKpis(UUID branchId) {

        long lowStock =
                inventoryItemRepository.findByBranchId(branchId)
                        .stream()
                        .filter(i -> (i.getQuantityOnHand() - i.getQuantityReserved()) <= 5)
                        .count();

        long outOfStock =
                inventoryItemRepository.findByBranchId(branchId)
                        .stream()
                        .filter(i -> (i.getQuantityOnHand() - i.getQuantityReserved()) <= 0)
                        .count();

        return DashboardSummaryDTO.OperationalKpis.builder()
                .salesCountToday(0L)
                .refundCountToday(0L)
                .lowStockCount(lowStock)
                .outOfStockCount(outOfStock)
                .deadStockValue(computeDeadStockValue())
                .build();
    }

    /* ============================================================
       REVENUE TREND (7 DAYS)
    ============================================================ */
    private Map<String, List<ChartPoint>> build7DayTrends() {

        UUID revenueId = accounts.revenue();
        UUID cogsId = accounts.cogs();
        UUID vatId = accounts.outputVat();

        Set<UUID> trendAccounts = Set.of(revenueId, cogsId, vatId);

        Map<LocalDate, Map<UUID, BigDecimal>> data =
                fetch7DayMovements(trendAccounts);

        List<ChartPoint> revenueTrend = new ArrayList<>();
        List<ChartPoint> profitTrend = new ArrayList<>();
        List<ChartPoint> vatTrend = new ArrayList<>();

        LocalDate today = LocalDate.now();

        for (int i = 6; i >= 0; i--) {

            LocalDate date = today.minusDays(i);

            Map<UUID, BigDecimal> day =
                    data.getOrDefault(date, Collections.emptyMap());

            BigDecimal revenue =
                    day.getOrDefault(revenueId, BigDecimal.ZERO);

            BigDecimal cogs =
                    day.getOrDefault(cogsId, BigDecimal.ZERO);

            BigDecimal vat =
                    day.getOrDefault(vatId, BigDecimal.ZERO);

            revenueTrend.add(new ChartPoint(date.toString(), revenue));
            profitTrend.add(new ChartPoint(date.toString(), revenue.subtract(cogs)));
            vatTrend.add(new ChartPoint(date.toString(), vat));
        }

        Map<String, List<ChartPoint>> result = new LinkedHashMap<>();
        result.put("revenue", revenueTrend);
        result.put("profit", profitTrend);
        result.put("vat", vatTrend);

        return result;
    }

    private AgingBucketDTO computeARAging() {

        BigDecimal current = BigDecimal.ZERO;
        BigDecimal d30 = BigDecimal.ZERO;
        BigDecimal d60 = BigDecimal.ZERO;
        BigDecimal d90 = BigDecimal.ZERO;
        BigDecimal over90 = BigDecimal.ZERO;

        for (Object[] row : saleRepository.arAgingRaw()) {

            int days = ((Number) row[0]).intValue();
            BigDecimal balance = (BigDecimal) row[1];

            if (balance == null || balance.compareTo(BigDecimal.ZERO) <= 0) {
                continue;
            }

            if (days <= 30) current = current.add(balance);
            else if (days <= 60) d30 = d30.add(balance);
            else if (days <= 90) d60 = d60.add(balance);
            else if (days <= 120) d90 = d90.add(balance);
            else over90 = over90.add(balance);
        }

        return new AgingBucketDTO(current, d30, d60, d90, over90);
    }

    private AgingBucketDTO computeAPAging() {

        BigDecimal current = BigDecimal.ZERO;
        BigDecimal d30 = BigDecimal.ZERO;
        BigDecimal d60 = BigDecimal.ZERO;
        BigDecimal d90 = BigDecimal.ZERO;
        BigDecimal over90 = BigDecimal.ZERO;

        UUID apAccountId = accounts.accountsPayable();

        for (Object[] row : ledgerRepo.apAgingRaw(apAccountId, CREDIT, DEBIT)) {

            int days = ((Number) row[0]).intValue();
            BigDecimal balance = (BigDecimal) row[1];

            if (balance == null || balance.compareTo(BigDecimal.ZERO) <= 0)
                continue;

            if (days <= 30) current = current.add(balance);
            else if (days <= 60) d30 = d30.add(balance);
            else if (days <= 90) d60 = d60.add(balance);
            else if (days <= 120) d90 = d90.add(balance);
            else over90 = over90.add(balance);
        }

        return new AgingBucketDTO(current, d30, d60, d90, over90);
    }

    private BigDecimal computeDeadStockValue() {

        LocalDateTime cutoff = LocalDateTime.now().minusDays(60);

        return inventoryItemRepository.findDeadStock(cutoff)
                .stream()
                .map(i -> i.getAverageCost()
                        .multiply(BigDecimal.valueOf(i.getQuantityOnHand())))
                .reduce(BigDecimal.ZERO, BigDecimal::add);
    }

    private List<ChartPoint> top5ProfitableBatches() {

        return batchConsumptionRepository.topBatchProfitRaw()
                .stream()
                .map(row -> {

                    UUID batchId = (UUID) row[0];
                    BigDecimal revenue = (BigDecimal) row[1];
                    BigDecimal cost = (BigDecimal) row[2];

                    BigDecimal profit =
                            Optional.ofNullable(revenue).orElse(BigDecimal.ZERO)
                                    .subtract(Optional.ofNullable(cost).orElse(BigDecimal.ZERO));

                    return new AbstractMap.SimpleEntry<>(batchId, profit);
                })
                .sorted((a, b) -> b.getValue().compareTo(a.getValue()))
                .limit(5)
                .map(e -> new ChartPoint(e.getKey().toString(), e.getValue()))
                .toList();
    }

    private BigDecimal computeMonthlyBudgetVariance(UUID accountId) {

        LocalDate start = LocalDate.now().withDayOfMonth(1);
        LocalDate end = start.plusMonths(1).minusDays(1);

        BigDecimal actual = ledgerRepo.netMovementForAccount(
                accountId,
                start.atStartOfDay(),
                end.atTime(23,59,59),
                DEBIT_NORMAL,
                CREDIT_NORMAL,
                DEBIT,
                CREDIT
        );

        BigDecimal planned =
                budgetRepository
                        .findByAccountIdAndPeriodStartAndPeriodEnd(
                                accountId,
                                start,
                                end
                        )
                        .map(Budget::getPlannedAmount)
                        .orElse(BigDecimal.ZERO);

        return actual.subtract(planned);
    }

    private Map<UUID, BigDecimal> fetchMovements(
            Set<UUID> accountIds,
            LocalDateTime start,
            LocalDateTime end
    ) {

        List<Object[]> rows = ledgerRepo.netMovementForAccountsBetween(
                accountIds,
                start,
                end,
                DEBIT_NORMAL,
                CREDIT_NORMAL,
                DEBIT,
                CREDIT
        );

        Map<UUID, BigDecimal> result = new HashMap<>();

        for (Object[] row : rows) {
            result.put((UUID) row[0], (BigDecimal) row[1]);
        }

        // Ensure zero values for missing accounts
        for (UUID id : accountIds) {
            result.putIfAbsent(id, BigDecimal.ZERO);
        }

        return result;
    }

    private Map<LocalDate, Map<UUID, BigDecimal>> fetch7DayMovements(Set<UUID> accountIds) {

        LocalDate today = LocalDate.now();
        LocalDate startDate = today.minusDays(6);

        LocalDateTime start = startDate.atStartOfDay();
        LocalDateTime end = today.atTime(23,59,59);

        List<Object[]> rows =
                ledgerRepo.netMovementGroupedByDateAndAccount(
                        accountIds,
                        start,
                        end,
                        DEBIT_NORMAL,
                        CREDIT_NORMAL,
                        DEBIT,
                        CREDIT
                );

        Map<LocalDate, Map<UUID, BigDecimal>> result = new HashMap<>();

        for (Object[] row : rows) {

            Object rawDate = row[0];

            LocalDate date;

            if (rawDate instanceof java.sql.Date sqlDate) {
                date = sqlDate.toLocalDate();
            } else if (rawDate instanceof java.time.LocalDate localDate) {
                date = localDate;
            } else if (rawDate instanceof java.sql.Timestamp ts) {
                date = ts.toLocalDateTime().toLocalDate();
            } else {
                throw new IllegalStateException("Unexpected date type: " + rawDate.getClass());
            }

            UUID accountId = (UUID) row[1];
            BigDecimal amount = (BigDecimal) row[2];

            result
                    .computeIfAbsent(date, d -> new HashMap<>())
                    .put(accountId, amount);
        }

        return result;
    }
}