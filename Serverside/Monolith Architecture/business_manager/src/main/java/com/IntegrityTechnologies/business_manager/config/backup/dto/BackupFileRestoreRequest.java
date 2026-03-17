package com.IntegrityTechnologies.business_manager.config.backup.dto;

import lombok.Data;

@Data
public class BackupFileRestoreRequest {
    private String relativePath;
    private boolean force;
}