package com.IntegrityTechnologies.business_manager.modules.finance.tax.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.*;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.dto.VatFilingDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.mapper.TaxFilingMapper;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.repository.VatFilingRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.service.VatFilingService;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/tax/vat")
@RequiredArgsConstructor
public class VatController {

    private final VatFilingService filingService;
    private final VatFilingRepository filingRepo;

    @PostMapping("/file/{periodId}")
    public VatFilingDTO file(@PathVariable UUID periodId) {
        TaxPeriod period = new TaxPeriod();
        period.setId(periodId);
        return TaxFilingMapper.toDto(
                filingService.file(period, currentUser())
        );
    }
    @PostMapping("/pay/{filingId}")
    public void pay(
            @PathVariable UUID filingId,
            @RequestParam UUID accountId
    ) {

        VatFiling filing = filingRepo.findById(filingId)
                .orElseThrow(() -> new IllegalArgumentException("Filing not found"));

        filingService.markPaid(filing, currentUser(), accountId);
    }

    @GetMapping
    public List<VatFiling> list() {
        return filingRepo.findAll();
    }

    private String currentUser() {
        var auth = SecurityContextHolder.getContext().getAuthentication();
        return auth != null ? auth.getName() : "SYSTEM";
    }
}