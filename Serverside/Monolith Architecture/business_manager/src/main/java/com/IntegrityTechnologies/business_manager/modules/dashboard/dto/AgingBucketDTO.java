package com.IntegrityTechnologies.business_manager.modules.dashboard.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.math.BigDecimal;

@Data
@AllArgsConstructor
public class AgingBucketDTO {

    private BigDecimal current;
    private BigDecimal days30;
    private BigDecimal days60;
    private BigDecimal days90;
    private BigDecimal over90;
}