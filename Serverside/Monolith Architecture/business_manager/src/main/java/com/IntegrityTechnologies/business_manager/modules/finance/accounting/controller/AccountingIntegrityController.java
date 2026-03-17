package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.security.JournalIntegrityAudit;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.security.JournalIntegrityService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantSuperuserOnly;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.UUID;

@RestController
@RequestMapping("/api/accounting/integrity")
@RequiredArgsConstructor
@TenantSuperuserOnly
public class AccountingIntegrityController {

    private final JournalIntegrityService service;
    private final BranchTenantGuard branchTenantGuard;

    @GetMapping("/verify")
    public IntegrityResponse verifyNow(@RequestParam UUID branchId) {
        branchTenantGuard.validate(branchId);
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
        branchTenantGuard.validate(branchId);
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
        branchTenantGuard.validate(branchId);
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