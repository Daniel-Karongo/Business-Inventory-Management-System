package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto;

import lombok.Builder;

import java.util.List;

@Builder
public record VatFilingReadinessResponse(

        boolean ready,

        boolean periodClosed,

        boolean alreadyFiled,

        String message,

        List<String> warnings
) {
}