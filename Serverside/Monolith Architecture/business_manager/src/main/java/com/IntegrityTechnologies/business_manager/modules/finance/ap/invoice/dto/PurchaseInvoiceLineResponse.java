package com.IntegrityTechnologies.business_manager.modules.finance.ap.invoice.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
public class PurchaseInvoiceLineResponse {

    private UUID id;

    private UUID productId;

    private UUID productVariantId;

    private String productName;

    private String productSku;

    private String variantName;

    private Long quantity;

    private BigDecimal unitCost;

    private BigDecimal discountAmount;

    private BigDecimal vatAmount;

    private BigDecimal lineSubtotal;

    private BigDecimal lineTotal;
}