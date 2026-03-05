package com.IntegrityTechnologies.business_manager.modules.finance.accounting.service;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.BranchAccountingSettings;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.RevenueRecognitionMode;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.BranchAccountingSettingsRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class BranchAccountingSettingsService {

    private final BranchAccountingSettingsRepository repository;

    public RevenueRecognitionMode getMode(UUID branchId) {

        return repository.findByBranchId(branchId)
                .map(BranchAccountingSettings::getRevenueRecognitionMode)
                .orElse(RevenueRecognitionMode.DELIVERY);
    }

    @Transactional
    public void updateMode(UUID branchId, RevenueRecognitionMode mode) {

        BranchAccountingSettings settings =
                repository.findByBranchId(branchId)
                        .orElseGet(() ->
                                BranchAccountingSettings.builder()
                                        .branchId(branchId)
                                        .revenueRecognitionMode(mode)
                                        .build()
                        );

        settings.setRevenueRecognitionMode(mode);
        repository.save(settings);
    }
}