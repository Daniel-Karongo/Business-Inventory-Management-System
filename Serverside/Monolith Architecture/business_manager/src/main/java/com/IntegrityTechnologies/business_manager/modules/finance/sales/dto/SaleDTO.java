package com.IntegrityTechnologies.business_manager.modules.finance.sales.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.payment.dto.PaymentDTO;
import lombok.Builder;
import lombok.Data;
import java.util.List;
import java.util.UUID;
import java.time.LocalDateTime;
import java.math.BigDecimal;

@Data
@Builder
public class SaleDTO {
    private UUID id;
    private LocalDateTime createdAt;
    private String createdBy;

    private BigDecimal totalAmount;
    private BigDecimal totalTax;
    private BigDecimal totalDiscount;

    private String status;
    private UUID customerId;

    private List<SaleLineItemDTO> items;
    private List<PaymentDTO> payments;
    private SaleCustomerDTO customer;
}