package com.IntegrityTechnologies.business_manager.modules.stock.onboarding.dto;

import lombok.Data;

import java.util.List;
import java.util.UUID;

@Data
public class BulkStockOnboardingRequest {

    private List<StockOnboardingRequest> rows;

    /*
     * BULK LEVEL EXPENSES
     */
    private List<StockOnboardingRequest.OperationalExpenseInput>
            operationalExpenses;

    /*
     * BULK PAYMENT SETTINGS
     */
    private Boolean autoPayOperationalExpenses;

    private UUID fundingAccountId;
}