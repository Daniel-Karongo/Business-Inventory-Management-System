package com.IntegrityTechnologies.business_manager.modules.finance.tax.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.dto.TaxStatus;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.CorporateTaxFilingRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.TaxSystemStateRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.UUID;

@RestController
@RequestMapping("/api/tax")
@RequiredArgsConstructor
public class TaxStatusController {

    private final TaxSystemStateRepository taxSystemStateRepository;
    private final CorporateTaxFilingRepository filingRepository;

    @GetMapping("/system/status")
    public TaxStatus status(@RequestParam UUID branchId) {

        var state = taxSystemStateRepository
                .findByBranchId(branchId)
                .orElse(null);

        LocalDateTime lastAccrual =
                filingRepository
                        .findTopByBranchIdOrderByFiledAtDesc(branchId)
                        .map(f -> f.getFiledAt())
                        .orElse(null);

        if (state == null) {
            return new TaxStatus(false, null, null, lastAccrual, false);
        }

        return new TaxStatus(
                state.isVatEnabled(),
                state.getVatRate(),
                state.getCorporateTaxRate(),
                lastAccrual,
                state.isLocked()
        );
    }
}