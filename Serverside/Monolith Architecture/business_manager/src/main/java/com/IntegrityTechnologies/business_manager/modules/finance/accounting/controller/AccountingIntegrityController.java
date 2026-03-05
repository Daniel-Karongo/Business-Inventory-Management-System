package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.security.JournalIntegrityAudit;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.security.JournalIntegrityService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.UUID;

@RestController
@RequestMapping("/api/accounting/integrity")
@RequiredArgsConstructor
public class AccountingIntegrityController {

    private final JournalIntegrityService service;

    @GetMapping("/verify")
    public IntegrityResponse verifyNow(@RequestParam UUID branchId) {

        SecurityUtils.requireAtLeast(Role.ADMIN);

        var result = service.verifyChain(branchId);

        return new IntegrityResponse(
                result.valid(),
                result.journalCount(),
                result.brokenAtJournalId(),
                result.lastJournalHash(),
                LocalDateTime.now()
        );
    }

    @PostMapping("/audit")
    public IntegrityResponse runAndPersistAudit(@RequestParam UUID branchId) {

        SecurityUtils.requireAtLeast(Role.ADMIN);

        JournalIntegrityAudit audit =
                service.performAndPersistAudit(branchId);

        return new IntegrityResponse(
                audit.isValid(),
                audit.getJournalCount(),
                audit.getBrokenAtJournalId(),
                audit.getLastJournalHash(),
                audit.getVerifiedAt()
        );
    }

    @GetMapping("/latest")
    public IntegrityResponse latest(@RequestParam UUID branchId) {

        SecurityUtils.requireAtLeast(Role.ADMIN);

        JournalIntegrityAudit audit =
                service.getLatestAudit(branchId);

        if (audit == null) return null;

        return new IntegrityResponse(
                audit.isValid(),
                audit.getJournalCount(),
                audit.getBrokenAtJournalId(),
                audit.getLastJournalHash(),
                audit.getVerifiedAt()
        );
    }

    public record IntegrityResponse(
            boolean valid,
            long journalCount,
            UUID brokenAtJournalId,
            String lastJournalHash,
            LocalDateTime verifiedAt
    ) {}
}