package com.IntegrityTechnologies.business_manager.modules.finance.tax.events;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.events.AccountingPeriodClosedEvent;
import com.IntegrityTechnologies.business_manager.modules.finance.tax.service.CorporateTaxService;
import lombok.RequiredArgsConstructor;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class CorporateTaxAccrualListener {

    private final CorporateTaxService service;

    @EventListener
    public void onPeriodClosed(AccountingPeriodClosedEvent event) {

        service.accrueCorporateTax(
                event.periodId(),
                event.branchId(),
                event.start(),
                event.end(),
                "SYSTEM"
        );
    }
}