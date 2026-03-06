package com.IntegrityTechnologies.business_manager.modules.finance.accounting.replay;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.LedgerEntry;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerEntryDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.JournalPostedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.LedgerEntryRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class FinancialProjectionReplayService {

    private final LedgerEntryRepository ledgerRepo;
    private final ApplicationEventPublisher publisher;

    @Transactional
    public void replayBranch(UUID branchId) {

        int page = 0;
        int size = 500;

        while (true) {

            var entries =
                    ledgerRepo.streamBranchLedger(
                            branchId,
                            PageRequest.of(page, size)
                    );

            if (entries.isEmpty()) {
                break;
            }

            Map<UUID, List<LedgerEntry>> grouped =
                    entries.stream()
                            .collect(Collectors.groupingBy(
                                    e -> e.getJournalEntry().getId()
                            ));

            for (var group : grouped.values()) {

                UUID journalId =
                        group.get(0).getJournalEntry().getId();

                List<LedgerEntryDTO> payload =
                        group.stream()
                                .map(e -> new LedgerEntryDTO(
                                        e.getAccount().getId(),
                                        e.getDirection(),
                                        e.getAmount()
                                ))
                                .toList();

                publisher.publishEvent(
                        new JournalPostedEvent(
                                journalId,
                                branchId,
                                payload
                        )
                );
            }

            page++;
        }
    }
}