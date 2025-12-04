package com.IntegrityTechnologies.business_manager.config;

import com.IntegrityTechnologies.business_manager.modules.stock.inventory.service.InventoryService;
import lombok.RequiredArgsConstructor;
import org.springframework.core.env.Environment;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.time.DayOfWeek;
import java.time.LocalDate;

@Component
@RequiredArgsConstructor
public class InventorySnapshotScheduler {

    private final InventoryService inventoryService;
    private final Environment env;

    @Scheduled(cron = "${rollcall.absentees.mark.cron}")
    public void scheduledSnapshot() {
        if (!Boolean.parseBoolean(env.getProperty("inventory.snapshot.enabled"))) return;

        LocalDate today = LocalDate.now();
        String mode = env.getProperty("inventory.snapshot.mode", "daily");

        switch (mode.toLowerCase()) {
            case "daily" -> inventoryService.takeSnapshot(today);
            case "weekly" -> {
                DayOfWeek dow = DayOfWeek.valueOf(env.getProperty("inventory.snapshot.weekday", "MONDAY"));
                if (today.getDayOfWeek() == dow) inventoryService.takeSnapshot(today);
            }
            case "monthly" -> {
                int day = Integer.parseInt(env.getProperty("inventory.snapshot.monthday", "1"));
                if (today.getDayOfMonth() == day) inventoryService.takeSnapshot(today);
            }
            case "yearly" -> {
                int dayOfYear = Integer.parseInt(env.getProperty("inventory.snapshot.yearday", "1"));
                if (today.getDayOfYear() == dayOfYear) inventoryService.takeSnapshot(today);
            }
        }
    }
}