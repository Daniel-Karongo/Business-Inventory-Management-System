package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto;

import java.math.BigDecimal;

public record VatBreakdown(
        BigDecimal net,
        BigDecimal vat,
        BigDecimal gross
) {}