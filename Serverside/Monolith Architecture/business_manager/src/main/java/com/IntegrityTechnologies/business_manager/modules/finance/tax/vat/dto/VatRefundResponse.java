package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.enums.VatRefundStatus;
import lombok.Builder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Builder
public record VatRefundResponse(

        UUID id,

        UUID filingId,

        BigDecimal amount,

        VatRefundStatus status,

        String requestedBy,

        LocalDateTime requestedAt,

        String processedBy,

        LocalDateTime processedAt
) {
}