package com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.dto;

import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

@Data
public class SettleOperationalExpenseRequest {
    private UUID fundingAccountId;
    private BigDecimal amount;
    private LocalDate settlementDate;
    private String reference;
    private UUID sourceId;
}