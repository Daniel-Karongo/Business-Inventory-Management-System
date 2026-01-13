package com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountType;

public record CreateAccountRequest(
        String code,
        String name,
        AccountType type
) {}