package com.IntegrityTechnologies.business_manager.modules.finance.accounting.events;

import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class OutboxPublisher {

    private final OutboxProcessor processor;

    @Scheduled(fixedDelay = 1000)
    public void publishEvents() {
        processor.processEvents();
    }
}