package com.IntegrityTechnologies.business_manager.config.backup.repository;

import com.IntegrityTechnologies.business_manager.config.backup.model.DatabaseBackupHistory;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface DatabaseBackupHistoryRepository
        extends JpaRepository<DatabaseBackupHistory, UUID> {
}