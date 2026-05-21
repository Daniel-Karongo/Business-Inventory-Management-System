package com.IntegrityTechnologies.business_manager.modules.finance.ap.payment.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.AccountRole;
import lombok.Builder;
import lombok.Data;

import java.util.UUID;

@Data
@Builder
public class FundingAccountResponse {

    private UUID id;

    private String code;

    private String name;

    private AccountRole role;
}