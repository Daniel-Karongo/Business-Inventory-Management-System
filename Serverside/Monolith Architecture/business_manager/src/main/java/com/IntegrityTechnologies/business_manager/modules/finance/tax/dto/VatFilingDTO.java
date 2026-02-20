package com.IntegrityTechnologies.business_manager.modules.finance.tax.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class VatFilingDTO {

    private UUID id;
    private UUID periodId;
    private BigDecimal outputVat;
    private BigDecimal inputVat;
    private BigDecimal vatPayable;
    private boolean paid;
    private LocalDateTime filedAt;
}