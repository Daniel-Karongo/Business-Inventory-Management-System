package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.ManualJournalRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.ManualJournalService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import com.IntegrityTechnologies.business_manager.security.BranchTenantGuard;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/accounting/manual-journals")
@RequiredArgsConstructor
@TenantManagerOnly
public class ManualJournalController {

    private final ManualJournalService service;
    private final BranchTenantGuard branchTenantGuard;

    @PostMapping
    public void post(@RequestBody ManualJournalRequest request) {
        branchTenantGuard.validate(request.branchId());
        service.post(request);
    }
}