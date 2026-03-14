package com.IntegrityTechnologies.business_manager.modules.finance.tax.events;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.AccountingPeriodClosedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.service.CorporateTaxService;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.event.EventListener;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Component
@RequiredArgsConstructor
public class CorporateTaxAccrualListener {

    private final CorporateTaxService service;

    @KafkaListener(
            topics = "accounting-period-closed",
            groupId = "corporate-tax"
    )
    @Transactional
    @ConditionalOnProperty(name = "spring.kafka.enabled", havingValue = "true")
    public void handleKafka(AccountingPeriodClosedEvent event) {
        process(event);
    }

    @EventListener
    @Transactional
    public void handleSpring(AccountingPeriodClosedEvent event) {
        process(event);
    }

    private void process(AccountingPeriodClosedEvent event) {

        try {

            TenantContext.setTenantId(event.tenantId());

            service.accrueCorporateTax(
                    event.periodId(),
                    event.branchId(),
                    event.start(),
                    event.end(),
                    "SYSTEM"
            );

        } finally {
            TenantContext.clear();
        }
    }
}