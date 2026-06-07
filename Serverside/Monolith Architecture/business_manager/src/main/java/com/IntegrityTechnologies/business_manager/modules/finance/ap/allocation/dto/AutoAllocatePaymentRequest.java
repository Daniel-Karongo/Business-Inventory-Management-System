package com.IntegrityTechnologies.business_manager.modules.finance.ap.allocation.dto;

import jakarta.validation.constraints.DecimalMin;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
public class AutoAllocatePaymentRequest {

    @NotNull
    private UUID branchId;
    @NotNull
    private UUID supplierId;
    @NotNull
    private UUID paymentId;
    @NotNull
    @DecimalMin(value = "0.01")
    private BigDecimal amount;
    private List<UUID> targetInvoiceIds;
}