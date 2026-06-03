package com.IntegrityTechnologies.business_manager.modules.finance.tax.base.dto;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.enums.VatCreditTreatment;
import lombok.AllArgsConstructor;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Data
@AllArgsConstructor
public class TaxStatus {

    private boolean vatEnabled;

    private BigDecimal vatRate;

    private BigDecimal corporateTaxRate;

    private VatCreditTreatment vatCreditTreatment;

    private LocalDateTime lastFilingDate;

    private boolean locked;
}