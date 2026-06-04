package com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.service;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.dto.VatCreditMovementResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.vat.repository.VatCreditMovementRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class VatCreditMovementQueryService {

    private final VatCreditMovementRepository repository;

    public List<VatCreditMovementResponse> list(
            UUID branchId
    ) {

        return repository
                .findByTenantIdAndBranchIdOrderByCreatedAtDesc(
                        TenantContext.getTenantId(),
                        branchId
                )
                .stream()
                .map(
                        c ->
                                VatCreditMovementResponse.builder()
                                        .id(
                                                c.getId()
                                        )
                                        .filingId(
                                                c.getFiling() != null
                                                        ? c.getFiling().getId()
                                                        : null
                                        )
                                        .type(
                                                c.getType()
                                        )
                                        .amount(
                                                c.getAmount()
                                        )
                                        .createdAt(
                                                c.getCreatedAt()
                                        )
                                        .build()
                )
                .toList();
    }
}