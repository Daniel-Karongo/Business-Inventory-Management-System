package com.IntegrityTechnologies.business_manager.modules.finance.budgeting.config;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Getter
@Setter
@Component
@ConfigurationProperties(prefix = "budget.snapshot")
public class BudgetSnapshotProperties {

    private String time = "02:00";
}