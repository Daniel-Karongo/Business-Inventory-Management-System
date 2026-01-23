package com.IntegrityTechnologies.business_manager.modules.dashboard.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.math.BigDecimal;

@Data
@AllArgsConstructor
public class ChartPoint {
    private String label;
    private BigDecimal value;
}