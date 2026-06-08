package com.IntegrityTechnologies.business_manager.modules.finance.ap.expense.dto;

import lombok.Data;

import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

@Data
public class BulkSettleOperationalExpenseRequest {

    private List<UUID> expenseIds;

    private UUID fundingAccountId;

    private LocalDate settlementDate;

    private String reference;

    private UUID sourceId;
}