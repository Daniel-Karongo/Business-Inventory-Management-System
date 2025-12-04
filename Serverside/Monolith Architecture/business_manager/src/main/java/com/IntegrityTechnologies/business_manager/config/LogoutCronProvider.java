package com.IntegrityTechnologies.business_manager.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalTime;

@Component
public class LogoutCronProvider {

    @Value("${rollcall.logout.time}")
    private String logoutTimeString;

    public String getCron() {
        // logoutTimeString = "18:30"
        LocalTime t = LocalTime.parse(logoutTimeString);

        int minute = t.getMinute();
        int hour = t.getHour();

        // Run once per day at HH:mm
        return String.format("0 %d %d * * *", minute, hour);
    }
}