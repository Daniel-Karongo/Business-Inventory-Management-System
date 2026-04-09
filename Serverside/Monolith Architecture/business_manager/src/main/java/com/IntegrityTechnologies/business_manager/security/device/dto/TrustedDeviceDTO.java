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
    private String fingerprint;
    private Boolean approved;
    private LocalDateTime firstSeenAt;
    private LocalDateTime lastSeenAt;
    private List<UUID> usedByUserIds;
}