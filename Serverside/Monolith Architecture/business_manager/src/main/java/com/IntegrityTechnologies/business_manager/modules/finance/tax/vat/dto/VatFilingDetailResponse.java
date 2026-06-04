package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.enums.VatBusinessStatus;
import lombok.Builder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Builder
public record VatFilingDetailResponse(

        UUID filingId,

        UUID periodId,

        BigDecimal outputVat,

        BigDecimal inputVat,

        BigDecimal openingCredit,

        BigDecimal creditApplied,

        BigDecimal closingCredit,

        BigDecimal vatPayable,

        BigDecimal vatReceivableCreated,

        BigDecimal paidAmount,

        BigDecimal outstandingAmount,

        VatBusinessStatus businessStatus,

        String displayStatus,

        String summaryMessage,

        boolean paid,

        LocalDateTime filedAt,

        LocalDateTime paidAt
) {
}