package com.IntegrityTechnologies.business_manager.modules.finance.ap.debt.projection;

import java.math.BigDecimal;
import java.util.UUID;

public interface SupplierBillLineProjection {

    UUID getInvoiceId();

    String getProductName();

    String getVariantName();

    Long getQuantity();

    BigDecimal getUnitCost();

    BigDecimal getTotalCost();
}