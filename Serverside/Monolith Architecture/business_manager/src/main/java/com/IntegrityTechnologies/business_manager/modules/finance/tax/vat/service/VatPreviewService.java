package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.AccountingPeriod;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto.VatFilingPreviewResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class VatPreviewService {

    private final VatReportService reportService;
    private final VatQueryService vatQueryService;

    public VatFilingPreviewResponse preview(
            AccountingPeriod period,
            UUID branchId
    ) {

        var report =
                reportService.generate(
                        period.getStartDate().atStartOfDay(),
                        period.getEndDate().atTime(
                                23,
                                59,
                                59
                        ),
                        branchId
                );

        BigDecimal outputVat =
                report.outputVat();

        BigDecimal inputVat =
                report.inputVat();

        BigDecimal openingCredit =
                vatQueryService.currentCredit(
                        branchId
                );

        BigDecimal currentPosition =
                outputVat.subtract(
                        inputVat
                );

        BigDecimal creditApplied =
                openingCredit.min(
                        currentPosition.max(
                                BigDecimal.ZERO
                        )
                );

        BigDecimal netVatDue =
                currentPosition.subtract(
                        creditApplied
                );

        BigDecimal generatedCredit =
                netVatDue.compareTo(
                        BigDecimal.ZERO
                ) < 0
                        ? netVatDue.abs()
                        : BigDecimal.ZERO;

        List<String> warnings =
                new ArrayList<>();

        if (
                creditApplied.compareTo(
                        BigDecimal.ZERO
                ) > 0
        ) {

            warnings.add(
                    "Existing VAT credit will be applied automatically."
            );
        }

        if (
                generatedCredit.compareTo(
                        BigDecimal.ZERO
                ) > 0
        ) {

            warnings.add(
                    "This filing will generate a new VAT credit."
            );
        }

        String outcome;

        String action;

        if (
                netVatDue.compareTo(
                        BigDecimal.ZERO
                ) > 0
        ) {

            outcome =
                    "VAT PAYMENT REQUIRED";

            action =
                    "PAY";
        }
        else if (
                generatedCredit.compareTo(
                        BigDecimal.ZERO
                ) > 0
        ) {

            outcome =
                    "VAT CREDIT GENERATED";

            action =
                    "CARRY_FORWARD";
        }
        else {

            outcome =
                    "NO VAT LIABILITY";

            action =
                    "NO_ACTION";
        }

        return VatFilingPreviewResponse
                .builder()
                .periodId(
                        period.getId()
                )
                .periodStart(
                        period.getStartDate()
                )
                .periodEnd(
                        period.getEndDate()
                )
                .outputVat(
                        outputVat
                )
                .inputVat(
                        inputVat
                )
                .openingCredit(
                        openingCredit
                )
                .creditApplied(
                        creditApplied
                )
                .netVatDue(
                        netVatDue.max(
                                BigDecimal.ZERO
                        )
                )
                .generatedCredit(
                        generatedCredit
                )
                .outcome(
                        outcome
                )
                .recommendedAction(
                        action
                )
                .warnings(
                        warnings
                )
                .build();
    }
}