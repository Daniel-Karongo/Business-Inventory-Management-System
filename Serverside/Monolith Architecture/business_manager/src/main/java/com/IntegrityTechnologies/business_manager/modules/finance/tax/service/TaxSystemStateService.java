package com.IntegrityTechnologies.business_manager.modules.finance.tax.service;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.config.TaxProperties;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.TaxSystemState;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.TaxSystemStateRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class TaxSystemStateService {

    private final TaxSystemStateRepository repository;
    private final TaxProperties defaults;

    @Transactional
    public TaxSystemState getOrCreate(UUID branchId) {

        return repository.findByBranchId(branchId)
                .orElseGet(() -> {

                    TaxSystemState state = new TaxSystemState();
                    state.setBranchId(branchId);

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
        return repository.findByBranchId(branchId)
                .orElseThrow(() ->
                        new IllegalStateException("Tax system not configured for branch " + branchId));
    }
}