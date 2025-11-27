package com.IntegrityTechnologies.business_manager.modules.sales.dto;

import lombok.Data;
import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Data
public class SaleRequest {
    private List<SaleLineDto> items;
    private List<PaymentDto> payments; // can be empty for POS later
    private BigDecimal totalAmount;
    private BigDecimal totalTax;
    private BigDecimal totalDiscount;
    private UUID customerId; // optional
    // metadata
    private String reference; // external reference if any
}

