package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.projection;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerEntryDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.JournalPostedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.repository.BudgetMonthlySnapshotRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.event.EventListener;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Component
@RequiredArgsConstructor
public class BudgetVarianceProjectionConsumer {

    private final BudgetMonthlySnapshotRepository snapshotRepository;

    @KafkaListener(
            topics = "journal-posted",
            groupId = "budget-variance"
    )
    @Transactional
    @ConditionalOnProperty(
            name = "spring.kafka.enabled",
            havingValue = "true",
            matchIfMissing = false
    )
    public void handleKafka(JournalPostedEvent event) {
        process(event);
    }

    @EventListener
    @Transactional
    public void handleSpring(JournalPostedEvent event) {
        process(event);
    }

    private void process(JournalPostedEvent event) {

        int year = LocalDate.now().getYear();
        int month = LocalDate.now().getMonthValue();

        for (LedgerEntryDTO entry : event.entries()) {

            BigDecimal delta =
                    entry.direction() == EntryDirection.DEBIT
                            ? entry.amount()
                            : entry.amount().negate();

            snapshotRepository.applyActualDelta(
                    event.branchId(),
                    year,
                    month,
                    entry.accountId(),
                    delta,
                    LocalDateTime.now()
            );
        }
    }
}