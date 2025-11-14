package com.IntegrityTechnologies.business_manager.modules.user.UserMapper;

import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.user.model.UserImage;
import com.IntegrityTechnologies.business_manager.modules.user.model.UserImageAudit;
import org.mapstruct.Mapper;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Mapper(componentModel = "spring")
public interface UserMapper {

    // --- ENTITY → DTO helper for User ---
    default String map(User user) {
        if (user == null) return null;
        return user.getId() + " | " + user.getUsername();
    }

    // --- DTO → ENTITY helper for User ---
    default User map(String value) {
        if (value == null || value.isBlank()) return null;

        User user = new User();
        try {
            String[] parts = value.split("\\|");
            if (parts.length > 0) {
                user.setId(UUID.fromString(parts[0].trim()));
            }
            if (parts.length > 1) {
                user.setUsername(parts[1].trim());
            }
        } catch (Exception e) {
            // fallback: treat entire value as username
            user.setUsername(value.trim());
        }
        return user;
    }

    // --- Map list of filenames to UserImage entities ---
    default List<UserImage> mapStringsToUserImages(List<String> filenames, User owner) {
        List<UserImage> images = new ArrayList<>();
        if (filenames == null || filenames.isEmpty()) return images;

        for (String file : filenames) {
            UserImage image = UserImage.builder()
                    .fileName(file)
                    .filePath(file) // Assuming path is same as filename here; adjust if full path needed
                    .user(owner)
                    .uploadedAt(LocalDateTime.now())
                    .deleted(false)
                    .build();
            images.add(image);
        }
        return images;
    }

    // --- Map UserImage entities to UserImageAudit entities ---
    default List<UserImageAudit> mapUserImagesToAudits(List<UserImage> images, String action, String reason, User performedBy) {
        List<UserImageAudit> audits = new ArrayList<>();
        if (images == null || images.isEmpty()) return audits;

        for (UserImage image : images) {
            UserImageAudit audit = UserImageAudit.builder()
                    .userId(image.getUser() != null ? image.getUser().getId() : null)
                    .fileName(image.getFileName())
                    .filePath(image.getFilePath())
                    .action(action)
                    .reason(reason)
                    .performedById(performedBy != null ? performedBy.getId() : null)
                    .performedByUsername(performedBy != null ? performedBy.getUsername() : null)
                    .timestamp(LocalDateTime.now())
                    .build();
            audits.add(audit);
        }
        return audits;
    }
}