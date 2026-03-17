package com.IntegrityTechnologies.business_manager.config.backup.controller;

import com.IntegrityTechnologies.business_manager.config.backup.dto.BackupFileRestoreRequest;
import com.IntegrityTechnologies.business_manager.config.backup.dto.DatabaseBackupResultDTO;
import com.IntegrityTechnologies.business_manager.config.backup.model.DatabaseBackupHistory;
import com.IntegrityTechnologies.business_manager.config.backup.repository.DatabaseBackupHistoryRepository;
import com.IntegrityTechnologies.business_manager.config.backup.service.DatabaseBackupService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformSuperAdminOnly;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/admin/backup")
@RequiredArgsConstructor
@PlatformSuperAdminOnly
public class DatabaseBackupController {

    private final DatabaseBackupHistoryRepository historyRepo;
    private final DatabaseBackupService backupService;

    @PostMapping("/run")
    public ResponseEntity<?> runBackup() {

        DatabaseBackupResultDTO result = backupService.backupNow();

        if (!result.isSuccess()) {
            return ResponseEntity
                    .status(500)
                    .body(result);
        }

        return ResponseEntity.ok(result);
    }

    @GetMapping
    public List<DatabaseBackupHistory> listBackups() {
        return historyRepo.findAll(Sort.by(Sort.Direction.DESC, "createdAt"));
    }

    @PostMapping("/restore/{id}")
    public ResponseEntity<?> restore(
            @PathVariable UUID id,
            @RequestParam(defaultValue = "false") boolean force
    ) throws IOException {
        backupService.restoreBackup(id, force);
        return ResponseEntity.ok("Database restored successfully");
    }

    @PostMapping("/restore/file")
    public ResponseEntity<?> restoreFromFile(
            @RequestBody BackupFileRestoreRequest request
    ) {
        backupService.restoreBackupFromFile(
                request.getRelativePath(),
                request.isForce()
        );

        return ResponseEntity.ok("Database restored from backup file");
    }
}