package com.IntegrityTechnologies.business_manager.modules.dashboard.service;

import com.IntegrityTechnologies.business_manager.modules.dashboard.dto.ActivityDTO;
import com.IntegrityTechnologies.business_manager.modules.dashboard.dto.ChartPoint;
import com.IntegrityTechnologies.business_manager.modules.dashboard.dto.DashboardSummaryDTO;
import com.IntegrityTechnologies.business_manager.modules.dashboard.repository.DashboardDailySnapshotRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.adapters.AccountingAccounts;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.BatchConsumptionRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventoryItemRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.StockTransactionRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryValuationService;
import com.IntegrityTechnologies.business_manager.modules.stock.product.parent.repository.ProductAuditRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;


@Service
@RequiredArgsConstructor
public class DashboardService {

    private final AccountingAccounts accounts;
    private final AccountBalanceRepository balanceRepo;
    private final InventoryValuationService valuationService;
    private final InventoryItemRepository inventoryItemRepository;
    private final BatchConsumptionRepository batchConsumptionRepository;
    private final DashboardDailySnapshotRepository snapshotRepo;
    private final DashboardSnapshotService snapshotService;
    private final StockTransactionRepository stockTransactionRepository;
    private final UserAuditRepository userAuditRepository;
    private final ProductAuditRepository productAuditRepository;
    private final SupplierAuditRepository supplierAuditRepository;
    private final BranchAuditRepository branchAuditRepository;
    private final DepartmentAuditRepository departmentAuditRepository;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }
    
    /* ============================================================
       MAIN ENTRY
    ============================================================ */
    public DashboardSummaryDTO getBranchDashboard(UUID branchId) {

        LocalDate today = LocalDate.now();

        snapshotService.compute(branchId, today);

        return DashboardSummaryDTO.builder()
                .branchId(branchId)
                .date(today)
                .financial(buildFinancialKpisFromSnapshots(branchId))
                .operational(buildOperationalKpis(branchId))
                .revenueTrend(buildSnapshotTrend(branchId, "revenue"))
                .profitTrend(buildSnapshotTrend(branchId, "profit"))
                .vatTrend(buildSnapshotTrend(branchId, "vat"))
                .topBatches(top5ProfitableBatches())
                .recentActivities(buildRecentActivities(branchId))
                .build();
    }

    /* ============================================================
       FINANCIAL KPIs (REAL-TIME BALANCE DRIVEN)
    ============================================================ */
    private DashboardSummaryDTO.FinancialKpis buildFinancialKpisFromSnapshots(UUID branchId) {

        LocalDate today = LocalDate.now();

        var snap =
                snapshotRepo.findByTenantIdAndBranchIdAndDate(tenantId(), branchId, today)
                        .orElseThrow(() ->
                                new IllegalStateException("Dashboard snapshot missing")
                        );

        BigDecimal inventoryValue =
                (BigDecimal) valuationService.getBranchValuation(branchId)
                        .get("totalValuation");

        BigDecimal ar =
                balanceRepo.findByTenantIdAndAccount_IdAndBranch_Id(
                        tenantId(),
                        accounts.get(tenantId(), branchId, AccountRole.ACCOUNTS_RECEIVABLE),
                        branchId)
                .map(b -> b.getBalance()).orElse(BigDecimal.ZERO);

        BigDecimal ap =
                balanceRepo.findByTenantIdAndAccount_IdAndBranch_Id(
                        tenantId(),
                        accounts.get(tenantId(), branchId, AccountRole.ACCOUNTS_PAYABLE),
                        branchId
                ).map(b -> b.getBalance()).orElse(BigDecimal.ZERO);

        BigDecimal vat =
                balanceRepo.findByTenantIdAndAccount_IdAndBranch_Id(
                        tenantId(),
                        accounts.get(tenantId(), branchId, AccountRole.VAT_PAYABLE),
                        branchId
                ).map(b -> b.getBalance()).orElse(BigDecimal.ZERO);

        return DashboardSummaryDTO.FinancialKpis.builder()
                .netRevenueToday(snap.getRevenue())
                .grossProfitToday(snap.getProfit())
                .vatPayable(vat)
                .accountsReceivable(ar)
                .accountsPayable(ap)
                .cashBalance(snap.getCash())
                .inventoryValue(inventoryValue)
                .build();
    }

    private List<ChartPoint> buildSnapshotTrend(UUID branchId, String metric) {

        LocalDate today = LocalDate.now();
        LocalDate start = today.minusDays(6);

        List<ChartPoint> result = new ArrayList<>();

        for (LocalDate d = start; !d.isAfter(today); d = d.plusDays(1)) {

            var snap =
                    snapshotRepo.findByTenantIdAndBranchIdAndDate(tenantId(), branchId, d)
                            .orElse(null);

            BigDecimal value = BigDecimal.ZERO;

            if (snap != null) {

                value = switch (metric) {
                    case "revenue" -> snap.getRevenue();
                    case "profit" -> snap.getProfit();
                    case "vat" -> snap.getVat();
                    default -> BigDecimal.ZERO;
                };
            }

            result.add(new ChartPoint(d.toString(), value));
        }

        return result;
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

    private List<ActivityDTO> buildRecentActivities(UUID branchId) {

        List<ActivityDTO> out = new ArrayList<>();

        stockTransactionRepository
                .findByBranchIdOrderByTimestampDesc(branchId)
                .stream()
                .limit(5)
                .forEach(tx ->
                        out.add(new ActivityDTO(
                                "STOCK",
                                tx.getType() + " " + tx.getQuantityDelta(),
                                tx.getPerformedBy(),
                                tx.getTimestamp()
                        ))
                );

        userAuditRepository.findTop5ByOrderByTimestampDesc()
                .forEach(a ->
                        out.add(new ActivityDTO(
                                "USER",
                                a.getAction() + " user " + a.getUsername(),
                                a.getPerformedByUsername(),
                                a.getTimestamp()
                        ))
                );
        productAuditRepository.findAll().stream()
                .sorted(Comparator.comparing(a -> a.getTimestamp(), Comparator.reverseOrder()))
                .limit(5)
                .forEach(a ->
                        out.add(new ActivityDTO(
                                "PRODUCT",
                                a.getAction() + " product " + a.getProductName(),
                                a.getPerformedBy(),
                                a.getTimestamp()
                        ))
                );

        supplierAuditRepository.findAll().stream()
                .sorted(Comparator.comparing(a -> a.getTimestamp(), Comparator.reverseOrder()))
                .limit(5)
                .forEach(a ->
                        out.add(new ActivityDTO(
                                "SUPPLIER",
                                a.getAction() + " supplier " + a.getSupplierName(),
                                a.getPerformedBy(),
                                a.getTimestamp()
                        ))
                );

        branchAuditRepository.findAll().stream()
                .sorted(Comparator.comparing(a -> a.getTimestamp(), Comparator.reverseOrder()))
                .limit(5)
                .forEach(a ->
                        out.add(new ActivityDTO(
                                "BRANCH",
                                a.getAction() + " branch " + a.getBranchName(),
                                a.getPerformedByUsername(),
                                a.getTimestamp()
                        ))
                );

        departmentAuditRepository.findAll().stream()
                .sorted(Comparator.comparing(a -> a.getTimestamp(), Comparator.reverseOrder()))
                .limit(5)
                .forEach(a ->
                        out.add(new ActivityDTO(
                                "DEPARTMENT",
                                a.getAction() + " department " + a.getDepartmentName(),
                                a.getPerformedByUsername(),
                                a.getTimestamp()
                        ))
                );

        out.sort(Comparator.comparing(ActivityDTO::getTime).reversed());
        return out.stream().limit(10).toList();
    }
}