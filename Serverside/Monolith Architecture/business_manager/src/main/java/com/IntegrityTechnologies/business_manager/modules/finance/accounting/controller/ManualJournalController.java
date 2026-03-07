package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.ManualJournalRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.ManualJournalService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/accounting/manual-journals")
@RequiredArgsConstructor
@TenantUserOnly
public class ManualJournalController {

    private final ManualJournalService service;

    @PostMapping
    public void post(@RequestBody ManualJournalRequest request) {
        service.post(request);
    }
}