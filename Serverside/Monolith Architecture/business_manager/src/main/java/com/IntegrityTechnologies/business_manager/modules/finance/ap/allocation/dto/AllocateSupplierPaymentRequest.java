package com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.dto;

import jakarta.validation.constraints.DecimalMin;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class AllocateSupplierPaymentRequest {

    @NotNull
    private UUID branchId;

    @NotNull
    private UUID paymentId;

    @NotNull
    private UUID purchaseInvoiceId;

    @NotNull
    @DecimalMin(value = "0.01")
    private BigDecimal allocationAmount;
}