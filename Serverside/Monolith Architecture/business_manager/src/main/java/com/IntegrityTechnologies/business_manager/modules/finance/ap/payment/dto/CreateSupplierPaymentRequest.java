package com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.enums.SupplierPaymentMethod;
import jakarta.validation.constraints.DecimalMin;
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
    private UUID fundingAccountId;

    @NotNull
    @DecimalMin(value = "0.01")
    private BigDecimal amount;

    @NotNull
    private SupplierPaymentMethod method;

    private String reference;

    @NotNull
    private LocalDate paymentDate;
}