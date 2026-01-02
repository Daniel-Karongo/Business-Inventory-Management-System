package com.IntegrityTechnologies.business_manager.modules.person.function.auth.dto;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.UUID;

public record UserSessionDTO(
        UUID tokenId,
        UUID branchId,
        LocalDate loginDate,
        LocalDateTime loginTime,
        boolean current
) {}