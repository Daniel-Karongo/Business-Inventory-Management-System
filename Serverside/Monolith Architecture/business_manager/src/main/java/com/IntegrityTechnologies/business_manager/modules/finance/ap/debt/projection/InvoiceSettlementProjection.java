package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.projection;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

public interface InvoiceSettlementProjection {

    UUID getInvoiceId();

    UUID getPaymentId();

    String getPaymentNumber();

    LocalDate getPaymentDate();

    BigDecimal getAllocatedAmount();

    String getPaymentMethod();
}