package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.projection;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

public interface PaymentSettlementProjection {

    UUID getPaymentId();

    UUID getInvoiceId();

    String getBillNumber();

    LocalDate getInvoiceDate();

    BigDecimal getAllocatedAmount();
}