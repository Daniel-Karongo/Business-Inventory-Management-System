package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SupplierTimelineEntryDto {

    private LocalDate timestamp;

    private String activity;

    private String reference;

    private BigDecimal debitAmount;
    private BigDecimal creditAmount;

    private BigDecimal runningBalance;
}