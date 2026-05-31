package com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.UUID;

@Data
@Builder
public class FundingAccountResponse {
    private UUID id;
    private String code;
    private String name;
    private String role;
    private boolean active;
    private BigDecimal balance;
}