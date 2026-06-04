package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto;

import jakarta.validation.constraints.DecimalMin;
import jakarta.validation.constraints.NotNull;

import java.math.BigDecimal;
import java.util.UUID;

public record RecordVatPaymentRequest(

        @NotNull
        UUID fundingAccountId,

        @NotNull
        @DecimalMin("0.01")
        BigDecimal amount

) {}