package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SupplierBillLineDto {
    private String productName;
    private String variantName;

    private Long quantity;

    private BigDecimal unitCost;
    private BigDecimal totalCost;
}