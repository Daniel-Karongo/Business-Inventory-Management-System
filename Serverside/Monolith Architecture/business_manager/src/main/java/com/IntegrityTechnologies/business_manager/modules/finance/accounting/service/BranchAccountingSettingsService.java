package com.IntegrityTechnologies.business_manager.modules.finance.accounting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.BranchAccountingSettings;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.RevenueRecognitionMode;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.BranchAccountingSettingsRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class BranchAccountingSettingsService {

    private final BranchAccountingSettingsRepository repository;
    private final BranchTenantGuard branchTenantGuard;

    public RevenueRecognitionMode getMode(UUID branchId) {

        branchTenantGuard.validate(branchId);

        UUID tenantId = TenantContext.getTenantId();

        return repository.findByTenantIdAndBranchId(
                        tenantId,
                        branchId
                )
                .map(BranchAccountingSettings::getRevenueRecognitionMode)
                .orElse(RevenueRecognitionMode.DELIVERY);
    }

    @Transactional
    public void updateMode(UUID branchId, RevenueRecognitionMode mode) {

        branchTenantGuard.validate(branchId);

        UUID tenantId = TenantContext.getTenantId();

        BranchAccountingSettings settings =
                repository.findByTenantIdAndBranchId(
                                tenantId,
                                branchId
                        )
                        .orElseGet(() ->
                                BranchAccountingSettings.builder()
                                        .tenantId(tenantId)
                                        .branchId(branchId)
                                        .revenueRecognitionMode(mode)
                                        .build()
                        );

        settings.setRevenueRecognitionMode(mode);

        repository.save(settings);
    }
}