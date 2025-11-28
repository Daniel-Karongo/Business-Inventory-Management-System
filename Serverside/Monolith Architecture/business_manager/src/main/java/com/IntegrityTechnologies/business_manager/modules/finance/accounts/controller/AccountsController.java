package com.IntegrityTechnologies.business_manager.modules.finance.accounts.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounts.dto.JournalEntryRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.dto.JournalEntryResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.model.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.model.PartTransaction;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.AccountService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.JournalService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.PartTransactionService;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.net.URI;
import java.util.List;
import java.util.UUID;

@Tag(name = "Accounts")
@RestController
@RequestMapping("/api/accounts")
@RequiredArgsConstructor
public class AccountsController {

    private final AccountService accountService;
    private final JournalService journalService;
    private final PartTransactionService partTransactionService;

    @PreAuthorize("hasAnyRole('ADMIN','MANAGER')")
    @PostMapping("/gl")
    public ResponseEntity<Account> createAccount(@RequestBody Account account) {
        Account created = accountService.createAccount(account);
        return ResponseEntity.created(URI.create("/api/accounts/gl/" + created.getId())).body(created);
    }

    @PreAuthorize("hasAnyRole('ADMIN','MANAGER','CASHIER')")
    @GetMapping("/gl/{id}")
    public ResponseEntity<Account> getAccount(@PathVariable UUID id) {
        return ResponseEntity.ok(accountService.getAccount(id));
    }

    @PreAuthorize("hasAnyRole('ADMIN','MANAGER')")
    @PostMapping("/journal")
    public ResponseEntity<JournalEntryResponse> postJournal(@RequestBody JournalEntryRequest req, @RequestHeader(value = "X-User", required = false) String user) {
        String performedBy = user != null ? user : "SYSTEM";
        JournalEntryResponse resp = journalService.createJournalEntry(req, performedBy);
        return ResponseEntity.ok(resp);
    }

    @PreAuthorize("hasAnyRole('ADMIN','MANAGER')")
    @GetMapping("/journal")
    public ResponseEntity<List<JournalEntryResponse>> listJournals() {
        return ResponseEntity.ok(journalService.listJournalEntries());
    }

    @PreAuthorize("hasAnyRole('ADMIN','MANAGER','CASHIER')")
    @PostMapping("/part-trans")
    public ResponseEntity<PartTransaction> recordPartTransaction(@RequestBody PartTransaction tx) {
        PartTransaction saved = partTransactionService.record(tx);
        return ResponseEntity.ok(saved);
    }

    @PreAuthorize("hasAnyRole('ADMIN','MANAGER','CASHIER')")
    @GetMapping("/part-trans")
    public ResponseEntity<List<PartTransaction>> listPartTransactions(@RequestParam String module, @RequestParam UUID refId) {
        return ResponseEntity.ok(partTransactionService.listByModuleAndRef(module, refId));
    }
}