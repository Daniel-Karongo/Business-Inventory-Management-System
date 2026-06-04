package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.service;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto.VatFilingDetailResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto.VatFilingSummaryResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.mapper.VatStatusMapper;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.VatFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.model.enums.VatFilingStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository.VatFilingRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class VatHistoryService {

    private final VatFilingRepository filingRepository;

    public List<VatFilingSummaryResponse> history(
            UUID branchId
    ) {

        return filingRepository
                .findByTenantIdAndBranchId(
                        TenantContext.getTenantId(),
                        branchId
                )
                .stream()
                .map(this::summary)
                .toList();
    }

    public VatFilingDetailResponse detail(
            UUID filingId
    ) {

        VatFiling filing =
                filingRepository
                        .findByTenantIdAndId(
                                TenantContext.getTenantId(),
                                filingId
                        )
                        .orElseThrow();

        return detail(filing);
    }

    private VatFilingSummaryResponse summary(
            VatFiling filing
    ) {

        return VatFilingSummaryResponse.builder()
                .filingId(
                        filing.getId()
                )
                .periodId(
                        filing.getPeriod().getId()
                )
                .periodLabel(
                        filing.getPeriod().getStartDate()
                                + " - "
                                + filing.getPeriod().getEndDate()
                )
                .vatDue(
                        filing.getVatPayable().max(
                                BigDecimal.ZERO
                        )
                )
                .vatCredit(
                        filing.getClosingCredit()
                )
                .paidAmount(
                        filing.getPaidAmount()
                )
                .outstandingAmount(
                        filing.getOutstandingAmount()
                )
                .displayStatus(
                        VatStatusMapper.displayStatus(
                                filing.getStatus()
                        )
                )
                .filedAt(
                        filing.getFiledAt()
                )
                .build();
    }

    private VatFilingDetailResponse detail(
            VatFiling filing
    ) {

        VatFilingStatus status = filing.getStatus();

        if (status == null) {
            status = VatFilingStatus.PAID;
        }

        String summary;

        switch (status) {

            case VAT_PAYABLE ->
                    summary =
                            "VAT payment is required.";

            case PARTIALLY_PAID ->
                    summary =
                            "VAT payment has been partially completed.";

            case PAID ->
                    summary =
                            "VAT obligations for this filing have been settled.";

            case VAT_CREDIT_CARRIED_FORWARD ->
                    summary =
                            "VAT credit has been carried forward.";

            case VAT_REFUND_PENDING ->
                    summary =
                            "VAT refund request is awaiting processing.";

            case VAT_REFUNDED ->
                    summary =
                            "VAT refund has been completed.";

            default ->
                    summary =
                            "VAT filing completed.";
        }

        return VatFilingDetailResponse.builder()
                .filingId(
                        filing.getId()
                )
                .periodId(
                        filing.getPeriod().getId()
                )
                .outputVat(
                        filing.getOutputVat()
                )
                .inputVat(
                        filing.getInputVat()
                )
                .openingCredit(
                        filing.getOpeningCredit()
                )
                .creditApplied(
                        filing.getCreditApplied()
                )
                .closingCredit(
                        filing.getClosingCredit()
                )
                .vatPayable(
                        filing.getVatPayable()
                )
                .vatReceivableCreated(
                        filing.getVatReceivableCreated()
                )
                .paidAmount(
                        filing.getPaidAmount()
                )
                .outstandingAmount(
                        filing.getOutstandingAmount()
                )
                .businessStatus(
                        VatStatusMapper.businessStatus(
                                status
                        )
                )
                .displayStatus(
                        VatStatusMapper.displayStatus(
                                status
                        )
                )
                .summaryMessage(
                        summary
                )
                .paid(
                        filing.getOutstandingAmount()
                                .compareTo(
                                        BigDecimal.ZERO
                                ) == 0
                )
                .filedAt(
                        filing.getFiledAt()
                )
                .paidAt(
                        filing.getPaidAt()
                )
                .build();
    }
}