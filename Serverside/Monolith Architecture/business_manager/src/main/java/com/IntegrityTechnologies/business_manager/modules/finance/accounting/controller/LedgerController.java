package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerRowResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/accounting/ledger")
@RequiredArgsConstructor
public class LedgerController {

    private final LedgerEntryRepository ledgerRepo;

    @GetMapping("/{accountId}")
    public List<LedgerRowResponse> ledger(@PathVariable UUID accountId) {

        SecurityUtils.requireAtLeast(Role.EMPLOYEE);

        List<LedgerEntry> entries =
                ledgerRepo.findByAccountIdOrderByPostedAtAsc(accountId);

        BigDecimal running = BigDecimal.ZERO;
        List<LedgerRowResponse> response = new ArrayList<>();

        for (LedgerEntry e : entries) {

            if (e.getDirection() == EntryDirection.DEBIT) {
                running = running.add(e.getAmount());
            } else {
                running = running.subtract(e.getAmount());
            }

            response.add(
                    new LedgerRowResponse(
                            e.getJournalEntry().getId(),
                            e.getJournalEntry().getReference(),
                            e.getPostedAt(),
                            e.getDirection().name(),
                            e.getAmount(),
                            running
                    )
            );
        }

        return response;
    }
}