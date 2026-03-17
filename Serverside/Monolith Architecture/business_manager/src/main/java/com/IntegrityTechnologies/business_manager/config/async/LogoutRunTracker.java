package com.IntegrityTechnologies.business_manager.config;

import org.springframework.stereotype.Component;

import java.time.LocalDate;

@Component
public class LogoutRunTracker {

    private LocalDate lastRunDate = null;

    public boolean hasRunToday() {
        return LocalDate.now().equals(lastRunDate);
    }

    public void markRunToday() {
        lastRunDate = LocalDate.now();
    }
}