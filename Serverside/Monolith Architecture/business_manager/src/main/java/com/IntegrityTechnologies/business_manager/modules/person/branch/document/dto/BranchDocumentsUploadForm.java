package com.IntegrityTechnologies.business_manager.modules.person.branch.document.dto;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class BranchDocumentsUploadForm {
    private List<BranchDocumentUploadDTO> files = new ArrayList<>();
}