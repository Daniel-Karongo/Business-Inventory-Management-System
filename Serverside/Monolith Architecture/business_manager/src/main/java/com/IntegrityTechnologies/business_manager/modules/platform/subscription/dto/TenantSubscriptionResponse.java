package com.IntegrityTechnologies.business_manager.modules.platform.subscription.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;
import java.util.UUID;

@Data
@Builder
public class TenantSubscriptionResponse {

    private UUID tenantId;

    private String planCode;

    private String planName;

    private int maxUsers;

    private int maxBranches;

    private boolean inventoryEnabled;

    private boolean accountingEnabled;

    private boolean reportingEnabled;

    private LocalDate startDate;

}