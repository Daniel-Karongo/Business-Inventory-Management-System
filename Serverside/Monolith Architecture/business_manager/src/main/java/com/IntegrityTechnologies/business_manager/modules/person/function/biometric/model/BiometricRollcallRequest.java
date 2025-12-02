package com.IntegrityTechnologies.business_manager.modules.person.function.biometric.model;

import lombok.Data;
import java.util.UUID;

@Data
public class BiometricRollcallRequest {
    private UUID userId;
    private UUID departmentId;
    private UUID BranchId;
    private BiometricType type;
    private String templateBase64; // base64 encoded template from client/kiosk
}
