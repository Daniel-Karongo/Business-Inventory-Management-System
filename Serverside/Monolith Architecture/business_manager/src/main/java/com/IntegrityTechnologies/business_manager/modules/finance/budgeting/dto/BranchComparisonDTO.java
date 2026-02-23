package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
public class BranchComparisonDTO {

    private UUID branchId;
    private String branchName;

    private BigDecimal planned;
    private BigDecimal actual;
    private BigDecimal variance;
    private BigDecimal variancePercent;
}