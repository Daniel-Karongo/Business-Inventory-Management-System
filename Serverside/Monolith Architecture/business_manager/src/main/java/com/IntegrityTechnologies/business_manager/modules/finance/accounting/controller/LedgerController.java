package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerRowResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
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
    public Page<LedgerRowResponse> ledger(
            @PathVariable UUID accountId,
            @RequestParam(required = false) UUID branchId,
            @PageableDefault(size = 100) Pageable pageable
    ) {

        SecurityUtils.requireAtLeast(Role.EMPLOYEE);

        return ledgerRepo
                .findLedgerWithRunningBalance(accountId, branchId, pageable)
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