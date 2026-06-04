package com.IntegrityTechnologies.business_manager.security.auth.dto;

import lombok.Builder;

import java.util.List;

@Builder
public record SessionLimitInfoResponse(

        int limit,

        int activeSessions,

        List<UserSessionDTO> sessions
) {}