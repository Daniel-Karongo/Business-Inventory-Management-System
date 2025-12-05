package com.IntegrityTechnologies.business_manager.modules.finance.sales.dto;

import lombok.Builder;
import lombok.Data;
import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
public class SaleLineItemDTO {
    private UUID productVariantId;
    private String productName;
    private UUID branchId;
    private int quantity;
    private BigDecimal unitPrice;
    private BigDecimal lineTotal;
}