package com.IntegrityTechnologies.business_manager.modules.person.branch.document.dto;

import com.IntegrityTechnologies.business_manager.modules.person.branch.document.model.BranchDocumentType;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class BranchDocumentDTO {
    private UUID id;
    private String fileName;
    private String url;
    private BranchDocumentType documentType;
    private String description;
    private Boolean deleted;
    private LocalDateTime uploadedAt;
    private LocalDateTime deletedAt;
    private boolean image;
    private boolean pdf;
    private boolean logo;
}