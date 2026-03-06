package com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountBalance;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

public record AccountBalanceResponse(
        UUID accountId,
        UUID branchId,
        BigDecimal balance,
        LocalDateTime updatedAt
) {

    public static AccountBalanceResponse from(AccountBalance b) {

        return new AccountBalanceResponse(
                b.getAccount().getId(),
                b.getBranch().getId(),
                b.getBalance(),
                b.getUpdatedAt()
        );
    }
}