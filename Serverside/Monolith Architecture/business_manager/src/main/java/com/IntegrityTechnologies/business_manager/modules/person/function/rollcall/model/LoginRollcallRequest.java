package com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model;

import lombok.Data;
import java.util.UUID;

@Data
public class LoginRollcallRequest {
    private UUID userId;
    private UUID departmentId;
    private UUID branchId;
}
