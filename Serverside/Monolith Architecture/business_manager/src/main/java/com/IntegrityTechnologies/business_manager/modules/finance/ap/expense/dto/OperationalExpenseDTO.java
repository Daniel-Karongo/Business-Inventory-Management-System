package com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.domain.ExpenseStatus;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

@Data
@Builder
public class OperationalExpenseDTO {

    private UUID id;

    private UUID expenseAccountId;

    private String description;

    private BigDecimal amount;

    private BigDecimal settledAmount;

    private ExpenseStatus status;

    private LocalDate accountingDate;
}