package com.IntegrityTechnologies.business_manager.modules.person.user.dto;

import lombok.Data;

import java.util.List;

@Data
public class UserBulkWithFilesDTO {
    private List<UserDTO> users;
}
