package com.IntegrityTechnologies.business_manager.modules.finance.tax.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;
import java.util.UUID;

@Data
@Builder
public class TaxPeriodDTO {

    private UUID id;

    private LocalDate startDate;

    private LocalDate endDate;

    private boolean closed;

    private String closedBy;
}