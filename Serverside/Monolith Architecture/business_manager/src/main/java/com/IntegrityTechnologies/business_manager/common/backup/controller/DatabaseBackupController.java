package com.IntegrityTechnologies.business_manager.common.backup.controller;

import com.IntegrityTechnologies.business_manager.common.backup.dto.BackupFileRestoreRequest;
import com.IntegrityTechnologies.business_manager.common.backup.dto.DatabaseBackupResultDTO;
import com.IntegrityTechnologies.business_manager.common.backup.model.DatabaseBackupHistory;
import com.IntegrityTechnologies.business_manager.common.backup.repository.DatabaseBackupHistoryRepository;
import com.IntegrityTechnologies.business_manager.common.backup.service.DatabaseBackupService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/admin/backup")
@RequiredArgsConstructor
public class DatabaseBackupController {

    private final DatabaseBackupHistoryRepository historyRepo;
    private final DatabaseBackupService backupService;

    @PostMapping("/run")
    @PreAuthorize("hasRole('SUPERUSER')")
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
    @PreAuthorize("hasRole('SUPERUSER')")
    public List<DatabaseBackupHistory> listBackups() {
        return historyRepo.findAll(Sort.by(Sort.Direction.DESC, "createdAt"));
    }

    @PostMapping("/restore/{id}")
    @PreAuthorize("hasRole('SUPERUSER')")
    public ResponseEntity<?> restore(
            @PathVariable UUID id,
            @RequestParam(defaultValue = "false") boolean force
    ) throws IOException {
        backupService.restoreBackup(id, force);
        return ResponseEntity.ok("Database restored successfully");
    }

    @PostMapping("/restore/file")
    @PreAuthorize("hasRole('SUPERUSER')")
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