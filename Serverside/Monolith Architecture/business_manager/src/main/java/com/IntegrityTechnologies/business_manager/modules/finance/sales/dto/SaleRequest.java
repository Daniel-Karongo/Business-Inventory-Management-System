package com.IntegrityTechnologies.business_manager.modules.finance.sales.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.customer.dto.CustomerRequest;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

@Data
public class SaleRequest {
    private List<SaleLineDto> items;
    private List<PaymentDTO> payments;
    private BigDecimal totalAmount;
    private BigDecimal totalTax;
    private BigDecimal totalDiscount;
    private CustomerRequest customerIdentifiers;
    private String reference; // external reference if any
}