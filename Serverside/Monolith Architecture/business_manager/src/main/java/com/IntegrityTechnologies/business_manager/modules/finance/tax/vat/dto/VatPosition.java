package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto;

import lombok.Builder;

import java.math.BigDecimal;

@Builder
public record VatPosition(

        BigDecimal outputVat,

        BigDecimal inputVat,

        BigDecimal openingCredit,

        BigDecimal creditApplied,

        BigDecimal payable,

        BigDecimal closingCredit,

        BigDecimal receivable

) {}