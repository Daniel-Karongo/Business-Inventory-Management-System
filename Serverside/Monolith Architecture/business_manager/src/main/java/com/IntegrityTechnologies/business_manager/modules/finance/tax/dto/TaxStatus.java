package com.IntegrityTechnologies.business_manager.modules.finance.tax.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Data
@AllArgsConstructor
public class TaxStatus {

    private boolean vatEnabled;
    private BigDecimal vatRate;
    private BigDecimal corporateTaxRate;
    private LocalDateTime lastAccrualDate;
    private boolean locked;
}