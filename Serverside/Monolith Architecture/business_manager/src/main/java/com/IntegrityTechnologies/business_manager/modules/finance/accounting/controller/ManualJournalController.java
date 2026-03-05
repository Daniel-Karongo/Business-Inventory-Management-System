package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.ManualJournalRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.service.ManualJournalService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/accounting/manual-journals")
@RequiredArgsConstructor
public class ManualJournalController {

    private final ManualJournalService service;

    @PostMapping
    public void post(@RequestBody ManualJournalRequest request) {
        SecurityUtils.requireAtLeast(Role.EMPLOYEE);
        service.post(request);
    }
}