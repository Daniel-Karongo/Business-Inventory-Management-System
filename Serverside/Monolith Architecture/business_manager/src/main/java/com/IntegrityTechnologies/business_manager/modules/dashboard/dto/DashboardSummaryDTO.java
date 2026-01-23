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

    private Kpis kpis;
    private List<ChartPoint> salesByCategory;
    private List<ChartPoint> revenueTrend;
    private List<ActivityDTO> recentActivities;

    @Data
    @Builder
    public static class Kpis {
        private BigDecimal salesToday;
        private BigDecimal inventoryValue;
        private Long customers;
        private Long salesCount;
    }
}