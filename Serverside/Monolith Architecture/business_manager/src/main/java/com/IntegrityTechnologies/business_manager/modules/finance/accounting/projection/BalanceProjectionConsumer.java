package com.IntegrityTechnologies.business_manager.modules.finance.accounting.projection;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerEntryDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.JournalPostedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.data.jpa.repository.support.SimpleJpaRepository;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class BalanceProjectionConsumer {

    private final AccountBalanceRepository balanceRepo;
    private final ObjectMapper mapper;
    private final ProcessedKafkaEventRepository processedRepo;

    @KafkaListener(
            topics = "journal-posted",
            groupId = "balance-projection"
    )
    @Transactional
    public void handle(String payload) {

        try {

            JournalPostedEvent event =
                    mapper.readValue(payload, JournalPostedEvent.class);

            if (processedRepo.existsById(event.journalId()))
                return;

            for (LedgerEntryDTO entry : event.entries()) {

                BigDecimal delta =
                        entry.direction() == EntryDirection.DEBIT
                                ? entry.amount()
                                : entry.amount().negate();

                balanceRepo.applyDelta(
                        entry.accountId(),
                        event.branchId(),
                        delta
                );
            }

            ProcessedKafkaEvent processed = new ProcessedKafkaEvent();
            processed.setEventId(event.journalId());

            processedRepo.save(processed);

        } catch (Exception ex) {

            throw new RuntimeException("Projection failed", ex);
        }
    }
}