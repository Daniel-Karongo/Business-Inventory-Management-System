package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.projection;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerEntryDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.JournalPostedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.budgeting.repository.BudgetMonthlySnapshotRepository;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
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

    private final ObjectMapper mapper;
    private final BudgetMonthlySnapshotRepository snapshotRepository;

    @KafkaListener(
            topics = "journal-posted",
            groupId = "budget-variance"
    )
    @Transactional
    public void handle(String payload) {

        try {

            JournalPostedEvent event =
                    mapper.readValue(payload, JournalPostedEvent.class);

            int year = LocalDate.now().getYear();
            int month = LocalDate.now().getMonthValue();

            for (LedgerEntryDTO entry : event.entries()) {

                BigDecimal delta =
                        entry.direction().name().equals("DEBIT")
                                ? entry.amount()
                                : entry.amount().negate();

                int updated =
                        snapshotRepository.applyActualDelta(
                                event.branchId(),
                                year,
                                month,
                                entry.accountId(),
                                delta,
                                LocalDateTime.now()
                        );

                if (updated == 0) {
                    continue;
                }
            }

        } catch (Exception ex) {

            throw new RuntimeException("Budget variance projection failed", ex);
        }
    }
}