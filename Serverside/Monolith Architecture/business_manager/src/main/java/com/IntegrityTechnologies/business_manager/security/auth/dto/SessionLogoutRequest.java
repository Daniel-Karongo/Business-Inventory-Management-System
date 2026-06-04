package com.IntegrityTechnologies.business_manager.security.auth.dto;

import java.util.UUID;

public record SessionLogoutRequest(
        UUID tokenId
) {}