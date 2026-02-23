package com.IntegrityTechnologies.business_manager.modules.finance.tax.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.tax.domain.CorporateTaxFiling;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.service.CorporateTaxService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/tax/corporate")
@RequiredArgsConstructor
public class CorporateTaxController {

    private final CorporateTaxService service;

    @GetMapping
    public List<CorporateTaxFiling> list() {
        return service.listAll();
    }

    @PostMapping("/accrue/{periodId}")
    public CorporateTaxFiling accrue(
            @PathVariable UUID periodId,
            @RequestParam String from,
            @RequestParam String to
    ) {
        return service.accrueCorporateTax(
                periodId,
                LocalDateTime.parse(from),
                LocalDateTime.parse(to),
                "SYSTEM"
        );
    }

    @PostMapping("/pay/{filingId}")
    public void pay(
            @PathVariable UUID filingId,
            @RequestParam UUID accountId
    ) {
        service.markPaid(filingId, "SYSTEM", accountId);
    }
}