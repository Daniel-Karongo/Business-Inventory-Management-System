package com.IntegrityTechnologies.business_manager.modules.person.user.mapper;

import com.IntegrityTechnologies.business_manager.modules.person.user.dto.UserImageDTO;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.UserImage;

import java.util.List;

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

    public static String resolveProfileThumbnail(
            List<UserImage> images
    ) {

        if (images == null || images.isEmpty()) {
            return null;
        }

        // 1️⃣ explicit thumbnail (CORRECT SOURCE OF TRUTH)
        String explicitThumbnail =
                images.stream()
                        .filter(i -> !Boolean.TRUE.equals(i.getDeleted()))
                        .filter(i -> Boolean.TRUE.equals(i.getProfileThumbnail()))
                        .findFirst()
                        .map(UserImage::getFilePath)
                        .orElse(null);

        if (explicitThumbnail != null) {
            return explicitThumbnail;
        }

        // 2️⃣ fallback: passport
        String passport =
                images.stream()
                        .filter(i -> !Boolean.TRUE.equals(i.getDeleted()))
                        .filter(i ->
                                "passport".equalsIgnoreCase(i.getFileDescription())
                        )
                        .findFirst()
                        .map(UserImage::getFilePath)
                        .orElse(null);

        if (passport != null) {
            return passport;
        }

        // 3️⃣ fallback: id
        String id =
                images.stream()
                        .filter(i -> !Boolean.TRUE.equals(i.getDeleted()))
                        .filter(i ->
                                "id".equalsIgnoreCase(i.getFileDescription())
                        )
                        .findFirst()
                        .map(UserImage::getFilePath)
                        .orElse(null);

        if (id != null) {
            return id;
        }

        // 4️⃣ fallback: any image
        return images.stream()
                .filter(i -> !Boolean.TRUE.equals(i.getDeleted()))
                .filter(i ->
                        i.getFileName().matches(".*\\.(jpg|jpeg|png|webp)$")
                )
                .findFirst()
                .map(UserImage::getFilePath)
                .orElse(null);
    }
}