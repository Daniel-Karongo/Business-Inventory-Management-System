package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.projection;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerEntryDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.JournalPostedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.config.kafka.ProcessedKafkaEvent;
import com.IntegrityTechnologies.business_manager.config.kafka.ProcessedKafkaEventRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.repository.BudgetMonthlySnapshotRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.event.EventListener;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class BudgetVarianceProjectionConsumer {

    private final BudgetMonthlySnapshotRepository snapshotRepository;
    private final ProcessedKafkaEventRepository processedRepo;

    private static final String CONSUMER = "BUDGET_VARIANCE";

    @KafkaListener(topics = "journal-posted", groupId = "budget-variance")
    @Transactional
    public void handleKafka(JournalPostedEvent event) {
        process(event);
    }

    @EventListener
    @Transactional
    @ConditionalOnProperty(
            name = "spring.kafka.enabled",
            havingValue = "false",
            matchIfMissing = true
    )
    public void handleSpring(JournalPostedEvent event) {
        process(event);
    }

    private void process(JournalPostedEvent event) {

        if (processedRepo.existsByTenantIdAndEventIdAndConsumer(
                event.tenantId(),
                event.journalId(),
                CONSUMER
        )) return;

        try {

            TenantContext.setTenantId(event.tenantId());

            UUID tenantId = event.tenantId();
            UUID branchId = event.branchId();

            int year = LocalDate.now().getYear();
            int month = LocalDate.now().getMonthValue();

            for (LedgerEntryDTO entry : event.entries()) {

                BigDecimal delta =
                        entry.direction() == EntryDirection.DEBIT
                                ? entry.amount()
                                : entry.amount().negate();

                snapshotRepository.applyActualDelta(
                        tenantId,
                        branchId,
                        year,
                        month,
                        entry.accountId(),
                        delta,
                        LocalDateTime.now()
                );
            }

            ProcessedKafkaEvent processed = new ProcessedKafkaEvent();
            processed.setEventId(event.journalId());
            processed.setConsumer(CONSUMER);
            processedRepo.save(processed);

        } finally {
            TenantContext.clear();
        }
    }
}