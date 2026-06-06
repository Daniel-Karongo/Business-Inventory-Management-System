package com.IntegrityTechnologies.business_manager.modules.person.branch.document.dto;

import com.IntegrityTechnologies.business_manager.modules.person.branch.document.model.BranchDocumentType;
import lombok.Data;
import org.springframework.web.multipart.MultipartFile;

@Data
public class BranchDocumentUploadDTO {
    private MultipartFile file;
    private BranchDocumentType documentType;
    private String description;
}