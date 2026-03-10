package com.IntegrityTechnologies.business_manager.modules.platform.identity.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Data
@Builder
public class PlatformUserResponse {

    private UUID id;

    private String username;

    private String role;

    private boolean active;

    private boolean locked;

    private List<String> emailAddresses;

    private List<String> phoneNumbers;

    private String idNumber;

    private LocalDateTime createdAt;

    private LocalDateTime updatedAt;

}