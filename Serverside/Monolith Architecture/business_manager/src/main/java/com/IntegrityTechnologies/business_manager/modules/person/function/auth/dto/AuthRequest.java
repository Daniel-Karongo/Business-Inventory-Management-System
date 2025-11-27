package com.IntegrityTechnologies.business_manager.modules.person.function.auth.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class AuthRequest {
    private String identifier;
    private String password;
}