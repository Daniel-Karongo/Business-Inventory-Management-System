package com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

@Data
@Builder
public class OperationalExpenseSettlementDTO {

    private UUID id;

    private UUID expenseId;

    private UUID fundingAccountId;

    private BigDecimal amount;

    private LocalDate settlementDate;

    private String reference;

    private boolean reversed;
}