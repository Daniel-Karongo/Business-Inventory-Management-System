package com.IntegrityTechnologies.business_manager.common.backup.model;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "database_backup_history")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class DatabaseBackupHistory {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    private String fileName;

    @Column(length = 1024)
    private String filePath;

    private long fileSize;

    private boolean encrypted;

    private boolean success;

    @Column(length = 1000)
    private String failureReason;

    private LocalDateTime createdAt;
}