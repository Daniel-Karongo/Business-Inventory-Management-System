package com.IntegrityTechnologies.business_manager.modules.finance.payment.base.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.enums.SupplierPaymentStatus;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class SupplierPaymentResponse {

    private UUID id;

    private String documentNumber;

    private UUID supplierId;

    private BigDecimal amount;

    private BigDecimal allocatedAmount;

    private BigDecimal unappliedAmount;

    private boolean fullyAllocated;

    private SupplierPaymentStatus status;

    private String method;

    private String reference;

    private boolean posted;

    private UUID journalEntryId;

    private LocalDate paymentDate;

    private LocalDateTime paidAt;

    private String paidBy;

    private boolean reversed;

    private LocalDateTime reversedAt;

    private String reversedBy;

    private String reversalReason;
}