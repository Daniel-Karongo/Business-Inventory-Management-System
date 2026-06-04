package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto;

import lombok.Builder;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Builder
public record VatPaymentResponse(

        UUID id,

        UUID filingId,

        BigDecimal amount,

        UUID fundingAccountId,

        String recordedBy,

        LocalDateTime recordedAt

) {}