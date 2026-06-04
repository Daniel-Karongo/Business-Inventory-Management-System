package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.enums.VatBusinessStatus;
import lombok.Builder;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.UUID;

@Builder
public record VatDashboardResponse(

        BigDecimal currentVatPosition,
        BigDecimal outstandingVat,
        BigDecimal availableCredit,
        BigDecimal pendingRefundAmount,
        BigDecimal totalVatPaid,

        boolean hasLatestFiling,
        UUID latestFilingId,
        LocalDate latestFiledPeriodStart,
        LocalDate latestFiledPeriodEnd,
        VatBusinessStatus latestFilingStatus,

        boolean filingRequired,
        UUID nextUnfiledPeriodId,
        LocalDate nextUnfiledPeriodStart,
        LocalDate nextUnfiledPeriodEnd,

        BigDecimal vatRate
) {
}