package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.api.AccountingFacade;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.JournalResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.JournalReversalRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerLineResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.JournalEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/accounting/journals")
@RequiredArgsConstructor
public class JournalController {

    private final JournalEntryRepository repo;
    private final AccountingFacade accountingFacade;

    @GetMapping
    public List<JournalResponse> list() {
        SecurityUtils.requireAtLeast(Role.EMPLOYEE);

        return repo.findAllByOrderByPostedAtDesc()
                .stream()
                .map(this::toResponse)
                .toList();
    }

    @GetMapping("/{id}")
    public JournalResponse get(@PathVariable UUID id) {
        SecurityUtils.requireAtLeast(Role.EMPLOYEE);

        JournalEntry j = repo.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Journal not found"));

        return toResponse(j);
    }

    @PostMapping("/{id}/reverse")
    @PreAuthorize("hasAnyRole('ADMIN','SUPERUSER')")
    public void reverse(
            @PathVariable UUID id,
            @RequestBody JournalReversalRequest req
    ) {
        accountingFacade.reverseJournal(id, req.reason(), SecurityUtils.currentUsername());
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