package com.IntegrityTechnologies.business_manager.modules.person.entity.user.mapper;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.dto.UserImageDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.UserImage;

public class UserImageMapper {

    public static UserImageDTO toDto(UserImage img) {

        String fileName = img.getFileName().toLowerCase();

        return UserImageDTO.builder()
                .id(img.getId())
                .fileName(img.getFileName())
                .url(img.getFilePath()) // already API-safe
                .description(img.getFileDescription())
                .deleted(img.getDeleted())
                .uploadedAt(img.getUploadedAt())
                .deletedAt(img.getDeletedAt())
                .pdf(fileName.endsWith(".pdf"))
                .image(
                        fileName.endsWith(".png") ||
                                fileName.endsWith(".jpg") ||
                                fileName.endsWith(".jpeg") ||
                                fileName.endsWith(".webp")
                )
                .build();
    }
}