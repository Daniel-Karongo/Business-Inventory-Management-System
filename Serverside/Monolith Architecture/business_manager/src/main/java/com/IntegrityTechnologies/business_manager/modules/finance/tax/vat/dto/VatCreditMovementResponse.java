package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.enums.VatCreditMovementType;
import lombok.Builder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Builder
public record VatCreditMovementResponse(

        UUID id,

        UUID filingId,

        VatCreditMovementType type,

        BigDecimal amount,

        LocalDateTime createdAt
) {
}