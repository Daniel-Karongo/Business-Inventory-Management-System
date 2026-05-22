package com.IntegrityTechnologies.business_manager.modules.finance.tax.dto;

import java.math.BigDecimal;

public record VatBreakdown(
        BigDecimal net,
        BigDecimal vat,
        BigDecimal gross
) {}