package com.IntegrityTechnologies.business_manager.modules.person.entity.user.dto;

import lombok.*;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UserImageDTO {

    private UUID id;

    private String fileName;

    /** Public API URL (NOT physical path) */
    private String url;

    private String description;

    private Boolean deleted;

    private LocalDateTime uploadedAt;

    private LocalDateTime deletedAt;

    /* Convenience flags for UI */
    private boolean pdf;
    private boolean image;
}