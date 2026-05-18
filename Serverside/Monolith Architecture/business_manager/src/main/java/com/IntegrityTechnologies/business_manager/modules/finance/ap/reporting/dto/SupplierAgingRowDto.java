package com.IntegrityTechnologies.business_manager.modules.finance.ap.reporting.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
public class SupplierAgingRowDto {

    private UUID supplierId;

    private String supplierName;

    private String branchName;

    private BigDecimal current;

    private BigDecimal days1To30;

    private BigDecimal days31To60;

    private BigDecimal days61To90;

    private BigDecimal days91To120;

    private BigDecimal over120Days;

    private BigDecimal totalOutstanding;
}