package com.IntegrityTechnologies.business_manager.modules.finance.accounting.engine;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerEntryDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.JournalPostedEvent;
import com.IntegrityTechnologies.business_manager.config.kafka.OutboxEventWriter;
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

        UUID branchId = journal.getBranchId();

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

        for (LedgerEntry entry : entries) {
            entry.markPosted(journal.getPostedAt());
        }

        UUID tenantId = journal.getTenantId();

        systemStateService.lockIfNecessary(branchId);

        String previousHash =
                journalRepo.findTopByTenantIdAndBranchIdAndPostedTrueAndIdNotOrderByPostedAtDescIdDesc(
                                tenantId,
                                branchId,
                                journal.getId()
                        )
                        .map(JournalEntry::getHash)
                        .orElse("GENESIS");

        journal.setPreviousHash(previousHash);

        /*
         * Temporary hash placeholder so deterministic hash can include fields.
         */
        journal.setHash("PENDING");

        String newHash =
                JournalHashUtil.computeJournalHash(
                        journal,
                        previousHash
                );

        journal.setHash(newHash);

        JournalEntry saved = journalRepo.save(journal);

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
                        saved.getBranchId(),
                        saved.getId(),
                        payloadEntries
                );

        outboxWriter.write(
                "JOURNAL_POSTED",
                saved.getBranchId(),
                event
        );

        return saved;
    }
}