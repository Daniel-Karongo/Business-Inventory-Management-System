package com.IntegrityTechnologies.business_manager.modules.finance.tax.base.mapper;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.dto.CorporateTaxFilingDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.model.CorporateTaxFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.corporate_tax.model.CorporateTaxFilingStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto.VatFilingDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.VatFiling;

public class TaxFilingMapper {

    public static VatFilingDTO toDto(VatFiling f) {

        return VatFilingDTO.builder()
                .id(f.getId())
                .periodId(f.getPeriod().getId())
                .outputVat(f.getOutputVat())
                .inputVat(f.getInputVat())
                .vatPayable(f.getVatPayable())
                .openingCredit(f.getOpeningCredit())
                .creditApplied(f.getCreditApplied())
                .closingCredit(f.getClosingCredit())
                .vatReceivableCreated(f.getVatReceivableCreated())
                .status(f.getStatus())
                .paid(f.isPaid())
                .filedAt(f.getFiledAt())
                .paidAt(f.getPaidAt())
                .build();
    }

    public static CorporateTaxFilingDTO toDto(
            CorporateTaxFiling f
    ) {

        return CorporateTaxFilingDTO
                .builder()
                .id(f.getId())
                .periodId(f.getPeriod().getId())

                .periodStart(
                        f.getPeriod()
                                .getStartDate()
                )

                .periodEnd(
                        f.getPeriod()
                                .getEndDate()
                )

                .taxableProfit(
                        f.getTaxableProfit()
                )

                .taxRate(
                        f.getTaxRate()
                )

                .taxAmount(
                        f.getTaxAmount()
                )

                .paidAmount(
                        f.getPaidAmount()
                )

                .outstandingAmount(
                        f.getOutstandingAmount()
                )

                .status(
                        f.getStatus()
                )

                .paid(
                        f.getStatus()
                                ==
                                CorporateTaxFilingStatus.PAID
                )

                .filedAt(
                        f.getFiledAt()
                )

                .paidAt(
                        f.getPaidAt()
                )

                .build();
    }
}