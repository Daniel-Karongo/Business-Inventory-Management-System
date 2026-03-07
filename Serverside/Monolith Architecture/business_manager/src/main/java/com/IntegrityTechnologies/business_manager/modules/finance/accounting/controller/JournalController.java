package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.JournalResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.JournalReversalRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerLineResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.GovernanceAuditService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.JournalEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/accounting/journals")
@RequiredArgsConstructor
@TenantManagerOnly
public class JournalController {

    private final JournalEntryRepository repo;
    private final AccountingFacade accountingFacade;
    private final GovernanceAuditService governanceAuditService;

    @GetMapping
    public Page<JournalResponse> list(
            @RequestParam UUID branchId,
            @PageableDefault(size = 50, sort = "postedAt") Pageable pageable
    ) {

        return repo.findByBranch_Id(branchId, pageable)
                .map(this::toResponse);
    }

    @GetMapping("/{id}")
    public JournalResponse get(@PathVariable UUID id) {

        JournalEntry j = repo.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Journal not found"));

        return toResponse(j);
    }

    @TenantAdminOnly
    @PostMapping("/{id}/reverse")
    public void reverse(
            @PathVariable UUID id,
            @RequestBody JournalReversalRequest req
    ) {

        JournalEntry journal = repo.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Journal not found"));

        UUID branchId = journal.getBranch().getId();
        String user = SecurityUtils.currentUsername();

        accountingFacade.reverseJournal(id, req.reason(), user);

        governanceAuditService.log(
                branchId,
                "JOURNAL_REVERSED",
                user,
                "Journal ID: " + id + " | Reason: " + req.reason()
        );
    }

    private JournalResponse toResponse(JournalEntry j) {

        return new JournalResponse(
                j.getId(),
                j.getReference(),
                j.getDescription(),
                j.getSourceModule(),
                j.getPostedBy(),
                j.getPostedAt(),
                j.getLedgerEntries().stream()
                        .map(e -> new LedgerLineResponse(
                                e.getAccount().getId(),
                                e.getAccount().getCode(),
                                e.getAccount().getName(),
                                e.getDirection().name(),
                                e.getAmount()
                        ))
                        .toList()
        );
    }
}