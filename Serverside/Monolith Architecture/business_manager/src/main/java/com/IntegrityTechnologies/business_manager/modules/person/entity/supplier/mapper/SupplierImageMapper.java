package com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.mapper;

import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto.SupplierImageDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.SupplierImage;

public class SupplierImageMapper {

    private SupplierImageMapper() {}

    public static SupplierImageDTO toDTO(SupplierImage image) {
        return SupplierImageDTO.builder()
                .id(image.getId())
                .fileName(image.getFileName())
                .filePath(image.getFilePath()) // optional; UI won’t render
                .description(image.getFileDescription())
                .deleted(image.getDeleted())
                .uploadedAt(image.getUploadedAt())
                .build();
    }
}