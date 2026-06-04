package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.service;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto.VatPaymentResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository.VatPaymentRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class VatPaymentQueryService {

    private final VatPaymentRepository paymentRepository;

    public List<VatPaymentResponse> list(
            UUID filingId
    ) {

        return paymentRepository
                .findByTenantIdAndFiling_Id(
                        TenantContext.getTenantId(),
                        filingId
                )
                .stream()
                .map(
                        p ->
                                VatPaymentResponse.builder()
                                        .id(
                                                p.getId()
                                        )
                                        .filingId(
                                                filingId
                                        )
                                        .amount(
                                                p.getAmount()
                                        )
                                        .fundingAccountId(
                                                p.getFundingAccountId()
                                        )
                                        .recordedBy(
                                                p.getRecordedBy()
                                        )
                                        .recordedAt(
                                                p.getRecordedAt()
                                        )
                                        .build()
                )
                .toList();
    }
}