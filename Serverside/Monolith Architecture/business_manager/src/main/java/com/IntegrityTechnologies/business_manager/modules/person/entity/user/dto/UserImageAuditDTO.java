package com.IntegrityTechnologies.business_manager.modules.person.entity.user.dto;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.UserImageAudit;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class UserImageAuditDTO {

    private UUID id;

    private String fileName;
    private String action;          // UPLOAD, DELETE, RESTORE
    private String reason;

    private UUID performedById;
    private String performedByUsername;

    private LocalDateTime timestamp;

    public static UserImageAuditDTO from(UserImageAudit a) {
        return UserImageAuditDTO.builder()
                .id(a.getId())
                .fileName(a.getFileName())
                .action(a.getAction())
                .reason(a.getReason())
                .performedById(a.getPerformedById())
                .performedByUsername(a.getPerformedByUsername())
                .timestamp(a.getTimestamp())
                .build();
    }
}