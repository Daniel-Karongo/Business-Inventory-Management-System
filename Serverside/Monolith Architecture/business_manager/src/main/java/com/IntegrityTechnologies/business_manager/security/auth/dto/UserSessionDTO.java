package com.IntegrityTechnologies.business_manager.security.auth.dto;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

public record UserSessionDTO(

        UUID tokenId,

        UUID branchId,

        String branchName,

        LocalDate loginDate,

        LocalDateTime loginTime,

        String deviceId,

        String deviceName,

        String browserName,

        String osName,

        String platform,

        String ipAddress,

        boolean current,
        boolean currentDevice
) {}