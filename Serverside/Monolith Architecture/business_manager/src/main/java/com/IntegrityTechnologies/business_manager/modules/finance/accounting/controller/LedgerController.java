package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerRowResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/accounting/ledger")
@RequiredArgsConstructor
@TenantManagerOnly
public class LedgerController {

    private final LedgerEntryRepository ledgerRepo;
    private final BranchTenantGuard branchTenantGuard;

    @GetMapping("/{accountId}")
    public Page<LedgerRowResponse> ledger(
            @PathVariable UUID accountId,
            @RequestParam(required = false) UUID branchId,
            @PageableDefault(size = 100) Pageable pageable
    ) {
        branchTenantGuard.validate(branchId);
        return ledgerRepo.findLedgerWithRunningBalance(
                        TenantContext.getTenantId(),
                        accountId,
                        branchId,
                        pageable
                )
                .map(p -> new LedgerRowResponse(
                        p.getJournalId(),
                        p.getReference(),
                        p.getPostedAt(),
                        p.getDirection(),
                        p.getAmount(),
                        p.getRunningBalance()
                ));
    }
}