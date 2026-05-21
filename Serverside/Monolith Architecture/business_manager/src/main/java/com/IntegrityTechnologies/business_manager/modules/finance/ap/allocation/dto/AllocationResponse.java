package com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.enums.PaymentAllocationStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AllocationResponse {
    private UUID allocationId;
    private UUID paymentId;
    private String paymentNumber;
    private UUID invoiceId;
    private String invoiceNumber;
    private BigDecimal allocatedAmount;
    private BigDecimal remainingInvoiceBalance;
    private BigDecimal remainingPaymentBalance;
    private LocalDateTime allocatedAt;
    private PaymentAllocationStatus status;
    private boolean reversed;
    private LocalDateTime reversedAt;
    private String reversedBy;
    private String reversalReason;
}