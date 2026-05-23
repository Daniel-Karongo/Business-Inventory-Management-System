package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.enums.PaymentAllocationStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PaymentSettlementDto {

    private UUID allocationId;

    private UUID invoiceId;

    private String billNumber;

    private LocalDate invoiceDate;

    private BigDecimal allocatedAmount;

    private PaymentAllocationStatus status;

    private boolean reversed;

    private LocalDateTime reversedAt;

    private String reversedBy;

    private String reversalReason;
}