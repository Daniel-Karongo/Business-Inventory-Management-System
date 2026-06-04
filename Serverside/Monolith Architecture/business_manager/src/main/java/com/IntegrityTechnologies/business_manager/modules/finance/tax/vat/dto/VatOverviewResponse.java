package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto;

import lombok.Builder;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

@Builder
public record VatOverviewResponse(

        BigDecimal vatToPay,
        BigDecimal creditAvailable,
        long refundsPending,

        boolean hasLastFiling,
        UUID lastFilingId,
        LocalDate lastReturnStart,
        LocalDate lastReturnEnd,

        boolean filingRequired,
        UUID nextReturnPeriodId,
        LocalDate nextReturnStart,
        LocalDate nextReturnEnd
) {
}