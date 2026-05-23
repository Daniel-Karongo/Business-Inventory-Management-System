package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.projection;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.enums.PaymentAllocationStatus;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

public interface PaymentSettlementProjection {

    UUID getAllocationId();

    UUID getPaymentId();

    UUID getInvoiceId();

    String getBillNumber();

    LocalDate getInvoiceDate();

    BigDecimal getAllocatedAmount();

    PaymentAllocationStatus getStatus();

    boolean getReversed();

    LocalDateTime getReversedAt();

    String getReversedBy();

    String getReversalReason();
}