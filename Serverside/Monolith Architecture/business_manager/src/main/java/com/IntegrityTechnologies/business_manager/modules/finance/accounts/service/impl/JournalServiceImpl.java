package com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.impl;

import com.IntegrityTechnologies.business_manager.modules.finance.accounts.dto.EntryLineRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.dto.JournalEntryRequest;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.dto.JournalEntryResponse;
import com.IntegrityTechnologies.business_manager.exception.AccountingException;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.model.Account;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.model.EntryLine;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.model.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.repository.AccountRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.repository.EntryLineRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.repository.JournalEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounts.service.JournalService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class JournalServiceImpl implements JournalService {

    private final JournalEntryRepository journalRepo;
    private final EntryLineRepository lineRepo;
    private final AccountRepository accountRepo;

    @Override
    @Transactional
    public JournalEntryResponse createJournalEntry(JournalEntryRequest req, String performedBy) {
        if (req.getLines() == null || req.getLines().isEmpty()) {
            throw new AccountingException("Journal must contain at least one line");
        }

        // compute totals
        BigDecimal totalDebit = BigDecimal.ZERO;
        BigDecimal totalCredit = BigDecimal.ZERO;
        for (EntryLineRequest l : req.getLines()) {
            if (l.getDebit() != null) totalDebit = totalDebit.add(l.getDebit());
            if (l.getCredit() != null) totalCredit = totalCredit.add(l.getCredit());
        }

        if (totalDebit.compareTo(totalCredit) != 0) {
            throw new AccountingException("Journal entry not balanced: debit=" + totalDebit + " credit=" + totalCredit);
        }

        // Build journal entry
        JournalEntry je = new JournalEntry();
        je.setReference(req.getReference() != null ? req.getReference() : "AUTO-" + UUID.randomUUID());
        je.setDescription(req.getDescription());
        je.setTimestamp(LocalDateTime.now());
        je.setCreatedBy(performedBy);

        List<EntryLine> persistedLines = new ArrayList<>();

        // create lines and update account balances
        for (EntryLineRequest reqLine : req.getLines()) {
            Account account = accountRepo.findById(reqLine.getAccountId())
                    .orElseThrow(() -> new AccountingException("Account not found: " + reqLine.getAccountId()));

            BigDecimal debit = reqLine.getDebit() == null ? BigDecimal.ZERO : reqLine.getDebit();
            BigDecimal credit = reqLine.getCredit() == null ? BigDecimal.ZERO : reqLine.getCredit();

            EntryLine line = new EntryLine();
            line.setJournalEntry(je);
            line.setAccount(account);
            line.setDebit(debit);
            line.setCredit(credit);
            line.setNote(reqLine.getNote());
            line.setTransactionType(reqLine.getTransactionType());

            // update account balance:
            // Simplified: balance = balance + (debit - credit)
            // Important: depending on account type, interpretation might differ (debit increases assets/expenses, credit increases liabilities/income)
            BigDecimal newBalance = account.getBalance().add(debit.subtract(credit));
            account.setBalance(newBalance);
            accountRepo.save(account);

            persistedLines.add(line);
        }

        je.setLines(persistedLines);
        JournalEntry saved = journalRepo.save(je);
        // lines will be cascaded; ensure they have journalEntry set
        persistedLines.forEach(line -> line.setJournalEntry(saved));
        lineRepo.saveAll(persistedLines);

        return toResponse(saved);
    }

    @Override
    @Transactional(readOnly = true)
    public JournalEntryResponse getJournalEntry(UUID id) {
        JournalEntry je = journalRepo.findById(id).orElseThrow(() -> new AccountingException("Journal not found: " + id));
        return toResponse(je);
    }

    @Override
    @Transactional(readOnly = true)
    public List<JournalEntryResponse> listJournalEntries() {
        List<JournalEntry> list = journalRepo.findAll();
        return list.stream().map(this::toResponse).collect(Collectors.toList());
    }

    private JournalEntryResponse toResponse(JournalEntry je) {
        List<JournalEntryResponse.EntryLineResponse> lr = je.getLines().stream().map(l -> {
            return JournalEntryResponse.EntryLineResponse.builder()
                    .id(l.getId())
                    .accountId(l.getAccount().getId())
                    .accountName(l.getAccount().getName())
                    .debit(l.getDebit())
                    .credit(l.getCredit())
                    .note(l.getNote())
                    .build();
        }).collect(Collectors.toList());

        return JournalEntryResponse.builder()
                .id(je.getId())
                .reference(je.getReference())
                .description(je.getDescription())
                .timestamp(je.getTimestamp())
                .createdBy(je.getCreatedBy())
                .lines(lr)
                .build();
    }
}