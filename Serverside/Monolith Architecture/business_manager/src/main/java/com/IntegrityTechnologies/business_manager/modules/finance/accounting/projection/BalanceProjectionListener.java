package com.IntegrityTechnologies.business_manager.modules.finance.accounting.projection;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerEntryDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.JournalPostedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.context.event.EventListener;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class BalanceProjectionListener {

    private final AccountBalanceRepository balanceRepo;
    private final BalanceProjectionCheckpointRepository checkpointRepo;

    @Transactional
    public void process(JournalPostedEvent event) {

        if (checkpointRepo.existsByJournalId(event.journalId()))
            return;

        Map<UUID, BigDecimal> deltaByAccount = new HashMap<>();

        for (LedgerEntryDTO entry : event.entries()) {

            BigDecimal delta =
                    entry.direction() == EntryDirection.DEBIT
                            ? entry.amount()
                            : entry.amount().negate();

            deltaByAccount.merge(
                    entry.accountId(),
                    delta,
                    BigDecimal::add
            );
        }

        for (var e : deltaByAccount.entrySet()) {

            balanceRepo.applyDelta(
                    e.getKey(),
                    event.branchId(),
                    e.getValue()
            );
        }

        BalanceProjectionCheckpoint checkpoint =
                new BalanceProjectionCheckpoint();

        checkpoint.setJournalId(event.journalId());
        checkpoint.setProcessedAt(LocalDateTime.now());

        checkpointRepo.save(checkpoint);
    }
}