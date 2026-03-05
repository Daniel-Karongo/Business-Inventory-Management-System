package com.IntegrityTechnologies.business_manager.modules.finance.accounting.security;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.JournalEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.JournalEntryRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class JournalIntegrityService {

    private final JournalEntryRepository journalRepo;
    private final JournalIntegrityAuditRepository auditRepository;

    @Transactional(readOnly = true)
    public IntegrityResult verifyChain(UUID branchId) {

        int page = 0;
        int size = 2000;

        String previous = "GENESIS";
        UUID brokenAt = null;

        long processed = 0;

        while (true) {

            var batch =
                    journalRepo.findByBranch_IdOrderByPostedAtAsc(
                            branchId,
                            PageRequest.of(page, size)
                    );

            if (!batch.hasContent())
                break;

            for (JournalEntry j : batch.getContent()) {

                String recalculated =
                        JournalHashUtil.computeJournalHash(j, previous);

                if (!recalculated.equals(j.getHash())) {

                    brokenAt = j.getId();

                    return new IntegrityResult(
                            false,
                            processed,
                            brokenAt,
                            j.getHash()
                    );
                }

                previous = j.getHash();
                processed++;
            }

            page++;
        }

        return new IntegrityResult(true, processed, null, previous);
    }

    @Transactional
    public JournalIntegrityAudit performAndPersistAudit(UUID branchId) {

        IntegrityResult result = verifyChain(branchId);

        JournalIntegrityAudit audit = new JournalIntegrityAudit();
        audit.setBranchId(branchId);
        audit.setVerifiedAt(LocalDateTime.now());
        audit.setValid(result.valid());
        audit.setJournalCount(result.journalCount());
        audit.setBrokenAtJournalId(result.brokenAtJournalId());
        audit.setLastJournalHash(result.lastJournalHash());

        return auditRepository.save(audit);
    }

    @Transactional(readOnly = true)
    public JournalIntegrityAudit getLatestAudit(UUID branchId) {
        return auditRepository.findTopByBranchIdOrderByVerifiedAtDesc(branchId)
                .orElse(null);
    }

    public record IntegrityResult(
            boolean valid,
            long journalCount,
            UUID brokenAtJournalId,
            String lastJournalHash
    ) {}
}