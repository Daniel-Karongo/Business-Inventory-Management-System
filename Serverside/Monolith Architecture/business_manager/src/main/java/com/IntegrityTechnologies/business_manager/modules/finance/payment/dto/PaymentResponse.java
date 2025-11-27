package com.IntegrityTechnologies.business_manager.modules.finance.payment.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class PaymentResponse {
    private UUID paymentId;
    private UUID saleId;
    private BigDecimal amount;
    private String method;
    private String status;
    private String providerReference;
    private String note;
    private LocalDateTime timestamp;
}