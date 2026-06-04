package com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.model.CorporateTaxFilingStatus;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class CorporateTaxFilingDTO {

    private UUID id;

    private UUID periodId;

    private LocalDate periodStart;

    private LocalDate periodEnd;

    private BigDecimal taxableProfit;

    private BigDecimal taxRate;

    private BigDecimal taxAmount;

    private BigDecimal paidAmount;

    private BigDecimal outstandingAmount;

    private CorporateTaxFilingStatus status;

    private boolean paid;

    private LocalDateTime filedAt;

    private LocalDateTime paidAt;
}