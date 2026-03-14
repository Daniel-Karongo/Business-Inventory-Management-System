package com.IntegrityTechnologies.business_manager.modules.finance.accounting.projection;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerEntryDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.JournalPostedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.event.EventListener;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;

@Component
@RequiredArgsConstructor
public class BalanceProjectionConsumer {

    private final AccountBalanceRepository balanceRepo;
    private final ProcessedKafkaEventRepository processedRepo;

    @KafkaListener(
            topics = "journal-posted",
            groupId = "balance-projection"
    )
    @Transactional
    @ConditionalOnProperty(name = "spring.kafka.enabled", havingValue = "true")
    public void handleKafka(JournalPostedEvent event) {
        process(event);
    }

    @EventListener
    @Transactional
    public void handleSpring(JournalPostedEvent event) {
        process(event);
    }

    private void process(JournalPostedEvent event) {

        if (processedRepo.existsByTenantIdAndEventId(
                event.tenantId(),
                event.journalId()
        )) return;

        for (LedgerEntryDTO entry : event.entries()) {

            BigDecimal delta =
                    entry.direction() == EntryDirection.DEBIT
                            ? entry.amount()
                            : entry.amount().negate();

            balanceRepo.applyDelta(
                    event.tenantId(),
                    entry.accountId(),
                    event.branchId(),
                    delta
            );
        }

        ProcessedKafkaEvent processed = new ProcessedKafkaEvent();
        processed.setEventId(event.journalId());

        processedRepo.save(processed);
    }
}