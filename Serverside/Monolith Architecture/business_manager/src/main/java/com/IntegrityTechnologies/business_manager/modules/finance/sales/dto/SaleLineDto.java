package com.IntegrityTechnologies.business_manager.modules.finance.sales.dto;

import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
public class SaleLineDto {
    private UUID productId;
    private Integer quantity;
    private BigDecimal unitPrice; // optional, service can look up product price
}
