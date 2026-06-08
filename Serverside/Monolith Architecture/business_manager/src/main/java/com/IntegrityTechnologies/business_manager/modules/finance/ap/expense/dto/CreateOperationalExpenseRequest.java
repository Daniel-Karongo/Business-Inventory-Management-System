package com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.dto;

import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

@Data
public class CreateOperationalExpenseRequest {

    private UUID branchId;

    private UUID expenseAccountId;

    private UUID fundingAccountId;

    private String description;

    private BigDecimal amount;

    private LocalDate accountingDate;

    private boolean autoPay;

    private String reference;

    private String sourceModule;

    private UUID sourceId;
}