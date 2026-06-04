package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto;

import lombok.Builder;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

@Builder
public record VatFilingPreviewResponse(

        UUID periodId,

        LocalDate periodStart,

        LocalDate periodEnd,

        BigDecimal outputVat,

        BigDecimal inputVat,

        BigDecimal openingCredit,

        BigDecimal creditApplied,

        BigDecimal netVatDue,

        BigDecimal generatedCredit,

        String outcome,

        String recommendedAction,

        List<String> warnings
) {
}