package com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.math.BigDecimal;
import java.util.UUID;

@Getter
@AllArgsConstructor
public class ReconciliationResult {

    private UUID accountId;
    private UUID branchId;
    private BigDecimal ledgerBalance;
    private BigDecimal projectedBalance;
    private boolean consistent;
}