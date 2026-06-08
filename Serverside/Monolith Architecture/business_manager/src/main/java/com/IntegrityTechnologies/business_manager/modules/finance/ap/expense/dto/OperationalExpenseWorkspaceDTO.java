package com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.dto;

import lombok.Builder;
import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
@Builder
public class OperationalExpenseWorkspaceDTO {

    private UUID expenseId;

    private String description;

    private UUID expenseAccountId;

    private String expenseAccountName;

    private OperationalExpenseDTO expense;

    private List<OperationalExpenseSettlementDTO> settlements;
}