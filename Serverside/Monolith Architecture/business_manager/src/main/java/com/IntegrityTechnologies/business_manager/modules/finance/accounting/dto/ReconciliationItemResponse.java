package com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.ReconciliationItem;

import java.math.BigDecimal;
import java.util.UUID;

public record ReconciliationItemResponse(

        UUID runId,
        UUID accountId,
        UUID branchId,
        BigDecimal ledgerBalance,
        BigDecimal projectedBalance,
        boolean consistent
) {

    public static ReconciliationItemResponse from(ReconciliationItem item) {

        return new ReconciliationItemResponse(
                item.getRunId(),
                item.getAccountId(),
                item.getBranchId(),
                item.getLedgerBalance(),
                item.getProjectedBalance(),
                item.isConsistent()
        );
    }
}