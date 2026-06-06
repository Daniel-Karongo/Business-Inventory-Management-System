package com.IntegrityTechnologies.business_manager.modules.person.branch.document.mapper;

import com.IntegrityTechnologies.business_manager.modules.person.branch.document.dto.BranchDocumentDTO;
import com.IntegrityTechnologies.business_manager.modules.person.branch.document.model.BranchDocument;
import lombok.experimental.UtilityClass;

@UtilityClass
public class BranchDocumentMapper {

    public BranchDocumentDTO toDto(
            BranchDocument doc
    ) {
        String fileName =
                doc.getFileName().toLowerCase();

        return BranchDocumentDTO.builder()
                .id(doc.getId())
                .fileName(doc.getFileName())
                .url(doc.getFilePath())
                .documentType(doc.getDocumentType())
                .description(doc.getDescription())
                .deleted(doc.isDeleted())
                .uploadedAt(doc.getUploadedAt())
                .deletedAt(doc.getDeletedAt())
                .logo(Boolean.TRUE.equals(doc.getLogo()))
                .pdf(fileName.endsWith(".pdf"))
                .image(
                        fileName.endsWith(".png")
                                || fileName.endsWith(".jpg")
                                || fileName.endsWith(".jpeg")
                                || fileName.endsWith(".webp")
                )
                .build();
    }

    public String resolveLogo(
            java.util.List<BranchDocument> docs
    ) {
        return docs.stream()
                .filter(d -> !d.isDeleted())
                .filter(d -> Boolean.TRUE.equals(d.getLogo()))
                .findFirst()
                .map(BranchDocument::getFilePath)
                .orElse(null);
    }
}