package com.IntegrityTechnologies.business_manager.modules.person.entity.user.model;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.dto.UserDTO;
import lombok.Data;

import java.util.List;

@Data
public class UserBulkWithFilesDTO {
    private List<UserDTO> users;
}
