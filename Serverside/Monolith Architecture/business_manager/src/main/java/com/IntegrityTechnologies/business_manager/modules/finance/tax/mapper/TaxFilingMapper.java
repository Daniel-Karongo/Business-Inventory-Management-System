package com.IntegrityTechnologies.business_manager.modules.finance.tax.mapper;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.*;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.dto.*;

public class TaxFilingMapper {

    public static VatFilingDTO toDto(VatFiling f) {
        return VatFilingDTO.builder()
                .id(f.getId())
                .periodId(f.getPeriod().getId())
                .outputVat(f.getOutputVat())
                .inputVat(f.getInputVat())
                .vatPayable(f.getVatPayable())
                .paid(f.isPaid())
                .filedAt(f.getFiledAt())
                .build();
    }

    public static CorporateTaxFilingDTO toDto(CorporateTaxFiling f) {
        return CorporateTaxFilingDTO.builder()
                .id(f.getId())
                .periodId(f.getPeriodId())
                .taxableProfit(f.getTaxableProfit())
                .taxRate(f.getTaxRate())
                .taxAmount(f.getTaxAmount())
                .paid(f.isPaid())
                .filedAt(f.getFiledAt())
                .paidAt(f.getPaidAt())
                .build();
    }
}