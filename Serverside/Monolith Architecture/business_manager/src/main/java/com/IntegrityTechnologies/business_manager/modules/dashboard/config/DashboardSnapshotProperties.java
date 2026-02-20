package com.IntegrityTechnologies.business_manager.modules.dashboard.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Getter
@Setter
@Component
@ConfigurationProperties(prefix = "dashboard.snapshot")
public class DashboardSnapshotProperties {

    private String time = "18:30";
}