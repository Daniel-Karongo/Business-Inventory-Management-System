package com.IntegrityTechnologies.business_manager.security.device.dto;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class DeviceStatsDTO {

    private long approvedDevices;
    private long pendingDevices;
    private long rejectedDevices;
    private long devicesInUse;
}