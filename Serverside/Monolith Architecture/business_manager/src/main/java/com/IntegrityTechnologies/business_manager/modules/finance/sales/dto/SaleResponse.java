package com.IntegrityTechnologies.business_manager.modules.finance.sales.dto;

import lombok.Builder;
import lombok.Data;
import java.util.UUID;
import java.time.LocalDateTime;

@Data
@Builder
public class SaleResponse {
    private UUID saleId;
    private LocalDateTime createdAt;
    private String createdBy;
    private String status;
    private Object summary; // expand as needed
}