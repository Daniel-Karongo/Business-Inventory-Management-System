package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.enums.VatFilingStatus;
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

    private BigDecimal openingCredit;

    private BigDecimal creditApplied;

    private BigDecimal closingCredit;

    private BigDecimal vatReceivableCreated;

    private BigDecimal paidAmount;

    private BigDecimal outstandingAmount;

    private VatFilingStatus status;

    private LocalDateTime filedAt;

    private LocalDateTime paidAt;
}