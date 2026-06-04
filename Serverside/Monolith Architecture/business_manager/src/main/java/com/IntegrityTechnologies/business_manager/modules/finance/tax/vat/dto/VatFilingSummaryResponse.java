package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto;

import lombok.Builder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Builder
public record VatFilingSummaryResponse(

        UUID filingId,

        UUID periodId,

        String periodLabel,

        BigDecimal vatDue,

        BigDecimal vatCredit,

        BigDecimal paidAmount,

        BigDecimal outstandingAmount,

        String displayStatus,

        LocalDateTime filedAt
) {
}