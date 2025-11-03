package com.IntegrityTechnologies.business_manager.modules.user.UserMapper;

import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import org.mapstruct.Mapper;

import java.util.UUID;

@Mapper(componentModel = "spring")
public interface UserMapper {

    // --- ENTITY → DTO helper ---
    default String map(User user) {
        if (user == null) return null;
        return user.getId() + " | " + user.getUsername();
    }

    // --- DTO → ENTITY helper ---
    default User map(String value) {
        if (value == null || value.isBlank()) return null;

        User user = new User();
        try {
            // Try parsing "id | username"
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
}