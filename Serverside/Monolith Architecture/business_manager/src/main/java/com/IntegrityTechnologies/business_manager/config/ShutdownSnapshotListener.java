package com.IntegrityTechnologies.business_manager.config;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.stereotype.Component;

import java.time.LocalDate;

@Component
@RequiredArgsConstructor
public class ShutdownSnapshotListener implements ApplicationListener<ContextClosedEvent> {

    private final InventoryService inventoryService;

    @Override
    public void onApplicationEvent(ContextClosedEvent event) {
        System.out.println("⚠ System shutting down — taking final inventory snapshot…");
        inventoryService.takeSnapshot(LocalDate.now());
    }
}
