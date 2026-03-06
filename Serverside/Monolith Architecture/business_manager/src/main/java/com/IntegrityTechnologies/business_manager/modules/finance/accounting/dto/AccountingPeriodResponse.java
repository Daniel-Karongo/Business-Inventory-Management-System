package com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;

import java.time.LocalDate;
import java.util.UUID;

public record AccountingPeriodResponse(

        UUID id,
        UUID branchId,
        LocalDate startDate,
        LocalDate endDate,
        boolean closed
) {

    public static AccountingPeriodResponse from(AccountingPeriod p) {

        return new AccountingPeriodResponse(
                p.getId(),
                p.getBranchId(),
                p.getStartDate(),
                p.getEndDate(),
                p.isClosed()
        );
    }
}