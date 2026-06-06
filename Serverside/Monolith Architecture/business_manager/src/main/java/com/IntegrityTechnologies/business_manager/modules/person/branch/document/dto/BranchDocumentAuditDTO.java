package com.IntegrityTechnologies.business_manager.modules.person.branch.document.dto;

import com.IntegrityTechnologies.business_manager.modules.person.branch.document.model.BranchDocumentAudit;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class BranchDocumentAuditDTO {

    private UUID id;
    private String fileName;
    private String action;
    private String reason;
    private UUID performedById;
    private String performedByUsername;
    private LocalDateTime timestamp;

    public static BranchDocumentAuditDTO from(
            BranchDocumentAudit a
    ) {
        return BranchDocumentAuditDTO.builder()
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