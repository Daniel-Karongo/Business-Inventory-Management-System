package com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.dto;

import jakarta.validation.constraints.DecimalMin;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class CreatePurchaseInvoiceLineRequest {

    @NotNull
    private UUID productId;

    private UUID productVariantId;

    @NotNull
    @Min(1)
    private Long quantity;

    @NotNull
    @DecimalMin(value = "0.00")
    private BigDecimal unitCost;

    private BigDecimal lineSubtotal;

    private BigDecimal lineTotal;

    @DecimalMin(value = "0.00")
    private BigDecimal discountAmount = BigDecimal.ZERO;

    @DecimalMin(value = "0.00")
    private BigDecimal vatAmount = BigDecimal.ZERO;
}