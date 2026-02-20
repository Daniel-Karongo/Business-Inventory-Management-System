package com.IntegrityTechnologies.business_manager.modules.dashboard.dto;

import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class DashboardSummaryDTO {

    private UUID branchId;
    private LocalDate date;

    private FinancialKpis financial;
    private OperationalKpis operational;

    private List<ChartPoint> revenueTrend;
    private List<ChartPoint> profitTrend;
    private List<ChartPoint> vatTrend;
    private List<ChartPoint> topBatches;

    private List<ActivityDTO> recentActivities;

    @Data
    @Builder
    public static class FinancialKpis {

        private BigDecimal netRevenueToday;
        private BigDecimal grossProfitToday;

        private BigDecimal vatPayable;
        private BigDecimal accountsReceivable;
        private BigDecimal accountsPayable;

        private BigDecimal cashBalance;
        private BigDecimal inventoryValue;

        private BigDecimal corporateTaxAccrued;

        private BigDecimal grossMarginPercent;
        private BigDecimal inventoryTurnover;
        private BigDecimal burnRate;
        private AgingBucketDTO arAging;
        private AgingBucketDTO apAging;
        private BigDecimal revenueBudgetVariance;
        private BigDecimal expenseBudgetVariance;
    }

    @Data
    @Builder
    public static class OperationalKpis {

        private Long salesCountToday;
        private Long refundCountToday;

        private Long lowStockCount;
        private Long outOfStockCount;

        private BigDecimal deadStockValue;
    }
}