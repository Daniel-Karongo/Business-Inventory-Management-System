package com.IntegrityTechnologies.business_manager.modules.finance.accounting.projection;

import com.IntegrityTechnologies.business_manager.config.kafka.ProcessedKafkaEventRepository;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.domain.enums.EntryDirection;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.LedgerEntryDTO;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.JournalPostedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.repository.AccountBalanceRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.event.EventListener;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class BalanceProjectionConsumer {

    private final AccountBalanceRepository balanceRepo;
    private final ProcessedKafkaEventRepository processedRepo;

    private static final String CONSUMER = "BALANCE_PROJECTION";

    @KafkaListener(
            topics = "journal-posted",
            groupId = "balance-projection"
    )
    @Transactional
    @ConditionalOnProperty(name = "spring.kafka.enabled", havingValue = "true")
    public void handleKafka(JournalPostedEvent event) {

        TenantContext.setTenantId(
                event.tenantId()
        );

        try {

            process(event);

        } finally {

            TenantContext.clear();
        }
    }

    @EventListener
    @Transactional
    @ConditionalOnProperty(
            name = "spring.kafka.enabled",
            havingValue = "false",
            matchIfMissing = true
    )
    public void handleSpring(JournalPostedEvent event) {

        TenantContext.setTenantId(
                event.tenantId()
        );

        try {

            process(event);

        } finally {

            TenantContext.clear();
        }
    }

    private void process(JournalPostedEvent event) {

        TenantContext.setTenantId(event.tenantId());

        int claimed =
                processedRepo.tryClaim(
                        UUID.randomUUID(),
                        event.tenantId(),
                        event.journalId(),
                        CONSUMER
                );

        if (claimed == 0) {
            return;
        }

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
    }
}