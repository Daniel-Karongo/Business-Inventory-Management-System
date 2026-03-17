package com.IntegrityTechnologies.business_manager.modules.finance.tax.service;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.config.TaxProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.TaxSystemState;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.TaxSystemStateRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class TaxSystemStateService {

    private final TaxSystemStateRepository repository;
    private final TaxProperties defaults;
    private final BranchTenantGuard branchTenantGuard;

    @Transactional
    public TaxSystemState getOrCreate(UUID branchId) {
        branchTenantGuard.validate(branchId);
        return repository.findByTenantIdAndBranchId(
                        TenantContext.getTenantId(),
                        branchId
                )
                .orElseGet(() -> {

                    TaxSystemState state = TaxSystemState.builder()
                            .tenantId(TenantContext.getTenantId())
                            .branchId(branchId)
                            .build();

                    // 🔥 Seed from YAML defaults
                    state.setTaxMode(defaults.getBusinessTaxMode());
                    state.setVatEnabled(defaults.isVatEnabled());
                    state.setVatRate(defaults.getVatRate());
                    state.setCorporateTaxRate(defaults.getCorporateTaxRate());
                    state.setLocked(false);

                    return repository.save(state);
                });
    }

    @Transactional(readOnly = true)
    public TaxSystemState get(UUID branchId) {
        branchTenantGuard.validate(branchId);
        return repository.findByTenantIdAndBranchId(
                        TenantContext.getTenantId(),
                        branchId
                )
                .orElseThrow(() ->
                        new IllegalStateException("Tax system not configured for branch " + branchId));
    }
}