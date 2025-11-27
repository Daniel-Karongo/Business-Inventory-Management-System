package com.IntegrityTechnologies.business_manager.modules.person.function.department.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class DepartmentUserDTO {
    private UUID id;
    private String username;
}