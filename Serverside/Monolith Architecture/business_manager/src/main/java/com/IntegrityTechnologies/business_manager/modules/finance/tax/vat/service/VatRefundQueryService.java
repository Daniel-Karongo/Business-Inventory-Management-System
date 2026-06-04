package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.service;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto.VatRefundResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository.VatRefundRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class VatRefundQueryService {

    private final VatRefundRepository repository;

    public List<VatRefundResponse> list(
            UUID branchId
    ) {

        return repository
                .findByTenantIdAndBranchId(
                        TenantContext.getTenantId(),
                        branchId
                )
                .stream()
                .map(
                        r ->
                                VatRefundResponse.builder()
                                        .id(
                                                r.getId()
                                        )
                                        .filingId(
                                                r.getFiling().getId()
                                        )
                                        .amount(
                                                r.getAmount()
                                        )
                                        .status(
                                                r.getStatus()
                                        )
                                        .requestedBy(
                                                r.getRequestedBy()
                                        )
                                        .requestedAt(
                                                r.getRequestedAt()
                                        )
                                        .processedBy(
                                                r.getProcessedBy()
                                        )
                                        .processedAt(
                                                r.getProcessedAt()
                                        )
                                        .build()
                )
                .toList();
    }
}