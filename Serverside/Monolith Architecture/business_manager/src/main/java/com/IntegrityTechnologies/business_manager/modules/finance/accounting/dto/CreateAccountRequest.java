package com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;

import java.util.UUID;

public record CreateAccountRequest(
        UUID branchId,
        String code,
        String name,
        AccountType type,
        AccountRole role
) {}