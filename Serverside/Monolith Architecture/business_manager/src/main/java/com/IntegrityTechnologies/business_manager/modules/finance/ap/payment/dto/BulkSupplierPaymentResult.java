package com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
public class BulkSupplierPaymentResult {

    private UUID supplierId;

    private String supplierName;

    private BigDecimal amountPaid;

    private String status;

    private String message;
}