package com.IntegrityTechnologies.business_manager.modules.dashboard.service;

import com.IntegrityTechnologies.business_manager.modules.dashboard.dto.*;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.model.Sale;
import com.IntegrityTechnologies.business_manager.modules.finance.sales.repository.SaleRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.repository.CustomerRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.model.InventorySnapshot;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.InventorySnapshotRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.repository.StockTransactionRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryValuationService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.*;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class DashboardService {

    private final SaleRepository saleRepository;
    private final CustomerRepository customerRepository;
    private final InventorySnapshotRepository snapshotRepository;
    private final StockTransactionRepository stockTransactionRepository;
    private final UserAuditRepository userAuditRepository;
    private final InventoryValuationService valuationService;

    /* ============================================================
       MAIN ENTRY
       ============================================================ */
    public DashboardSummaryDTO getBranchDashboard(UUID branchId) {

        LocalDate today = LocalDate.now();
        LocalDateTime start = today.atStartOfDay();
        LocalDateTime end = today.atTime(23, 59, 59);

        return DashboardSummaryDTO.builder()
                .branchId(branchId)
                .date(today)
                .kpis(buildKpis(branchId, start, end))
                .salesByCategory(buildSalesByCategory(branchId, start, end))
                .revenueTrend(getRevenueTrend(branchId))
                .recentActivities(buildRecentActivities(branchId))
                .build();
    }

    /* ============================================================
       KPIs
       ============================================================ */
    private DashboardSummaryDTO.Kpis buildKpis(
            UUID branchId,
            LocalDateTime start,
            LocalDateTime end
    ) {

        BigDecimal salesToday = saleRepository.findAll().stream()
                .filter(s -> s.getStatus() == Sale.SaleStatus.COMPLETED)
                .filter(s -> s.getCreatedAt() != null)
                .filter(s -> !s.getCreatedAt().isBefore(start) && !s.getCreatedAt().isAfter(end))
                .filter(s ->
                        s.getLineItems().stream()
                                .anyMatch(li -> branchId.equals(li.getBranchId()))
                )
                .map(Sale::getTotalAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal inventoryValue =
                (BigDecimal) valuationService.getBranchValuation(branchId)
                        .get("totalValuation");

        long customers = customerRepository.count();
        long salesCount = saleRepository.count();

        return DashboardSummaryDTO.Kpis.builder()
                .salesToday(salesToday)
                .inventoryValue(inventoryValue)
                .customers(customers)
                .salesCount(salesCount)
                .build();
    }

    /* ============================================================
       SALES BY CATEGORY (BAR / DONUT)
       ============================================================ */
    private List<ChartPoint> buildSalesByCategory(
            UUID branchId,
            LocalDateTime start,
            LocalDateTime end
    ) {

        Map<String, BigDecimal> totals = new HashMap<>();

        saleRepository.findAll().stream()
                .filter(s -> s.getStatus() == Sale.SaleStatus.COMPLETED)
                .filter(s -> s.getCreatedAt() != null)
                .filter(s -> !s.getCreatedAt().isBefore(start) && !s.getCreatedAt().isAfter(end))
                .forEach(sale ->
                        sale.getLineItems().forEach(li -> {
                            if (!branchId.equals(li.getBranchId())) return;
                            totals.merge(
                                    li.getProductName(),
                                    li.getLineTotal(),
                                    BigDecimal::add
                            );
                        })
                );

        return totals.entrySet().stream()
                .map(e -> new ChartPoint(e.getKey(), e.getValue()))
                .collect(Collectors.toList());
    }

    /* ============================================================
       REVENUE TREND (SNAPSHOT-BASED)
       ============================================================ */

    public List<ChartPoint> getRevenueTrend(UUID branchId) {

        return saleRepository.revenueTrendByDay(branchId)
                .stream()
                .map(row -> new ChartPoint(
                        row[0].toString(),        // yyyy-MM-dd
                        (BigDecimal) row[1]       // summed revenue
                ))
                .toList();
    }


    /* ============================================================
       ACTIVITY FEED
       ============================================================ */
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

        userAuditRepository.findAll().stream()
                .sorted(Comparator.comparing(a -> a.getTimestamp(), Comparator.reverseOrder()))
                .limit(5)
                .forEach(a ->
                        out.add(new ActivityDTO(
                                "USER",
                                a.getAction() + " user " + a.getUsername(),
                                a.getPerformedByUsername(),
                                a.getTimestamp()
                        ))
                );

        out.sort(Comparator.comparing(ActivityDTO::getTime).reversed());
        return out.stream().limit(10).toList();
    }
}