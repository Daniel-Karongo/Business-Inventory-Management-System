package com.IntegrityTechnologies.business_manager.modules.communication.notification.base;

import lombok.Data;

@Data
public class BranchNotificationSettingsDTO {

    private Boolean smsEnabled;

    private Boolean emailEnabled;

    private Boolean allowRetries;

    private Integer maxRetryCount;
}