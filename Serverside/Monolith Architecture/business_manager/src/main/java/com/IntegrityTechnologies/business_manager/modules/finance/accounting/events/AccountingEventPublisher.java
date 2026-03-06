package com.IntegrityTechnologies.business_manager.modules.finance.accounting.events;

public interface AccountingEventPublisher {

    void publish(String eventType, Object event);

}