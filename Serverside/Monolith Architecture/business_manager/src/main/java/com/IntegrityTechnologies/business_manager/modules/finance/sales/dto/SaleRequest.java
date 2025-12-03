package com.IntegrityTechnologies.business_manager.modules.finance.sales.dto;

import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.dto.CustomerRequest;
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
    private CustomerRequest customerIdentifiers;
    private String reference; // external reference if any
}