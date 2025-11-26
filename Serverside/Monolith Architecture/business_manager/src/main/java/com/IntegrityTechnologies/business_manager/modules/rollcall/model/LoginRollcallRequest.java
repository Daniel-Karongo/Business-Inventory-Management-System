package com.IntegrityTechnologies.business_manager.modules.rollcall.model;

import lombok.Data;
import java.util.UUID;

@Data
public class LoginRollcallRequest {
    private UUID userId;
    private UUID departmentId;
}
