package com.IntegrityTechnologies.business_manager.modules.finance.payment.base.dto;

import jakarta.validation.constraints.DecimalMin;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

@Data
public class CreateSupplierPaymentRequest {

    @NotNull
    private UUID branchId;

    @NotNull
    private UUID supplierId;

    @NotNull
    @DecimalMin(value = "0.01")
    private BigDecimal amount;

    @NotBlank
    private String method;

    private String reference;

    @NotNull
    private LocalDate paymentDate;
}