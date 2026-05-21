package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class InvoiceSettlementDto {
    private UUID paymentId;

    private String paymentNumber;
    private LocalDate paymentDate;

    private BigDecimal allocatedAmount;

    private String paymentMethod;
}