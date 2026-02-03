package com.IntegrityTechnologies.business_manager.common.backup.repository;

import com.IntegrityTechnologies.business_manager.common.backup.model.DatabaseBackupHistory;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

public interface DatabaseBackupHistoryRepository
        extends JpaRepository<DatabaseBackupHistory, UUID> {
}