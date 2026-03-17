package com.IntegrityTechnologies.business_manager.common.backup.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.UUID;

@Data
@Builder
public class DatabaseBackupResultDTO {
    private UUID id;
    private String fileName;
    private String filePath;
    private long fileSize;
    private boolean encrypted;
    private boolean success;
    private String failureReason;
    private LocalDateTime createdAt;
}