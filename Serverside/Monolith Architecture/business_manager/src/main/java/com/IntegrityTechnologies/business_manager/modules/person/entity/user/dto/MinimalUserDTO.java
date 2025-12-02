package com.IntegrityTechnologies.business_manager.modules.person.entity.user.dto;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class MinimalUserDTO {
    private UUID id;
    private String username;

    public static MinimalUserDTO from(User user) {
        MinimalUserDTO dto = new MinimalUserDTO();
        dto.setId(user.getId());
        dto.setUsername(user.getUsername());
        return dto;
    }
}