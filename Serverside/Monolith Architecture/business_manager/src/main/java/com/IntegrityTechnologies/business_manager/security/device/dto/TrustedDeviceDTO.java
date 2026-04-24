package com.IntegrityTechnologies.business_manager.security.device.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@Builder
public class TrustedDeviceDTO {

    private UUID id;
    private UUID branchId;
    private String deviceName;

    private String browserName;
    private String osName;
    private String platform;
    private String ipAddress;
    private String userAgent;

    private List<UUID> attemptedByUserIds;
    private Integer pendingAttemptCount;

    private String deviceId;
    private String status;
    private LocalDateTime firstSeenAt;
    private LocalDateTime lastSeenAt;
    private List<UUID> usedByUserIds;
    private List<String> usedByUsernames;
}