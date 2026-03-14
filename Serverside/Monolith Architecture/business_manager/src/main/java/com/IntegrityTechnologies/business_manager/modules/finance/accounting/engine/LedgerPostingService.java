package com.IntegrityTechnologies.business_manager.modules.finance.accounting.engine;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerEntryDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.JournalPostedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.OutboxEventWriter;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.AccountingSystemStateService;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.JournalEntryRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.security.JournalHashUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class LedgerPostingService {

    private final JournalEntryRepository journalRepo;
    private final AccountingSystemStateService systemStateService;
    private final OutboxEventWriter outboxWriter;

    @Transactional
    public JournalEntry post(
            JournalEntry journal,
            List<LedgerEntry> entries,
            String performedBy
    ) {

        if (journal.isPosted())
            throw new IllegalStateException("Journal already posted");

        if (entries == null || entries.isEmpty())
            throw new IllegalStateException("Journal must contain entries");

        if (!journal.getLedgerEntries().isEmpty())
            throw new IllegalStateException("Entries already attached");

        UUID branchId = journal.getBranch().getId();

        for (LedgerEntry e : entries) {

            if (!e.getAccount().getBranchId().equals(branchId)
        || !e.getAccount().getTenantId().equals(journal.getTenantId())) {

                throw new IllegalStateException(
                        "Ledger entry account belongs to another branch or tenant"
                );
            }
        }

        journal.getLedgerEntries().addAll(entries);

        journal.markPosted(performedBy);

        systemStateService.lockIfNecessary(branchId);

        JournalEntry saved = journalRepo.save(journal);
        UUID tenantId = saved.getBranch().getTenantId();
        branchId = saved.getBranch().getId();

        String previousHash =
                journalRepo
                        .findTopByTenantIdAndBranchIdAndPostedTrueAndIdNotOrderByPostedAtDescIdDesc(
                                tenantId,
                                branchId,
                                saved.getId()
                        )
                        .map(JournalEntry::getHash)
                        .orElse("GENESIS");

        String newHash =
                JournalHashUtil.computeJournalHash(saved, previousHash);

        saved.setPreviousHash(previousHash);
        saved.setHash(newHash);

        journalRepo.save(saved);

        List<LedgerEntryDTO> payloadEntries =
                entries.stream()
                        .map(e -> new LedgerEntryDTO(
                                e.getAccount().getId(),
                                e.getDirection(),
                                e.getAmount()
                        ))
                        .toList();

        JournalPostedEvent event =
                new JournalPostedEvent(
                        tenantId,
                        saved.getBranch().getId(),
                        saved.getId(),
                        payloadEntries
                );

        outboxWriter.write(
                "JOURNAL_POSTED",
                saved.getBranch().getId(),
                event
        );

        return saved;
    }
}