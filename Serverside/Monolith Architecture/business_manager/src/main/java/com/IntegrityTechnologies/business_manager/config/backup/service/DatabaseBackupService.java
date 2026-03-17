package com.IntegrityTechnologies.business_manager.common.backup.service;

import com.IntegrityTechnologies.business_manager.common.backup.config.DatabaseBackupProperties;
import com.IntegrityTechnologies.business_manager.common.backup.dto.DatabaseBackupResultDTO;
import com.IntegrityTechnologies.business_manager.common.backup.model.DatabaseBackupHistory;
import com.IntegrityTechnologies.business_manager.common.backup.repository.DatabaseBackupHistoryRepository;
import com.IntegrityTechnologies.business_manager.common.backup.util.BackupCompressionUtil;
import com.IntegrityTechnologies.business_manager.config.FileStorageService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.UUID;

/**
 * Database backup service.
 *
 * - Uses FileStorageService for ALL filesystem concerns
 * - Stores ONLY relative paths in the database
 * - Real mysqldump execution
 * - ZIP + optional AES encryption
 * - Retention by file count + total size
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class DatabaseBackupService {

    private static final DateTimeFormatter TS_FORMAT =
            DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss");

    private final DatabaseBackupProperties props;
    private final BackupEncryptionService encryptionService;
    private final BackupRetentionService retentionService;
    private final DatabaseBackupHistoryRepository historyRepo;
    private final FileStorageService fileStorageService;

    public DatabaseBackupResultDTO backupNow() {

        if (!props.isEnabled()) {
            throw new IllegalStateException("Database backup is disabled");
        }

        DatabaseBackupHistory history = new DatabaseBackupHistory();
        history.setCreatedAt(LocalDateTime.now());

        try {
            /* =========================================================
               Resolve backup directory (via FileStorageService)
               ========================================================= */

            Path backupRoot = fileStorageService.internalRoot(props.getDir());

            /* =========================================================
               Build readable filename
               ========================================================= */

            String timestamp = LocalDateTime.now().format(TS_FORMAT);
            String dbName = props.getMysql().getDatabase();
            String suffix = Integer.toHexString((int) (System.nanoTime() & 0xffff));

            String fileName = String.format(
                    "backup_%s_%s_%s.sql",
                    dbName,
                    timestamp,
                    suffix
            );

            Path sqlFile = backupRoot.resolve(fileName);

            /* =========================================================
               Execute mysqldump (atomic, crash-safe)
               ========================================================= */

            DatabaseBackupProperties.Mysql db = props.getMysql();

            // 1️⃣ Prepare temp file (same directory)
            Path tempFile = backupRoot.resolve(fileName + ".tmp");

            ProcessBuilder pb = new ProcessBuilder(
                    db.getBinPath() + "/mysqldump",
                    "-h", db.getHost(),
                    "-P", String.valueOf(db.getPort()),
                    "-u", db.getUsername(),
                    db.getDatabase()
            );

            // 2️⃣ Redirect output safely
            pb.redirectOutput(tempFile.toFile());
            pb.redirectError(ProcessBuilder.Redirect.INHERIT);

            // 3️⃣ Execute
            Process process = pb.start();
            int exit = process.waitFor();

            // 4️⃣ Handle failure
            if (exit != 0) {
                Files.deleteIfExists(tempFile);
                throw new IllegalStateException("mysqldump failed with exit code " + exit);
            }

            // 5️⃣ Atomically promote temp → final SQL
            Files.move(tempFile, sqlFile, StandardCopyOption.ATOMIC_MOVE);

            /* =========================================================
               Compress + encrypt
               ========================================================= */

            Path zipped = BackupCompressionUtil.zip(sqlFile);
            Path finalFile = encryptionService.encrypt(zipped);

            fileStorageService.secure(finalFile);

            /* =========================================================
               Persist history (RELATIVE PATH ONLY)
               ========================================================= */

            Path relativePath = fileStorageService.toRelative(finalFile);

            history.setFileName(finalFile.getFileName().toString());
            history.setFilePath(relativePath.toString().replace("\\", "/"));
            history.setFileSize(Files.size(finalFile));
            history.setEncrypted(props.getEncryption().isEnabled());
            history.setSuccess(true);

            historyRepo.save(history);

            /* =========================================================
               Enforce retention
               ========================================================= */

            retentionService.enforceRetention(
                    props.getRetention().getMaxFiles(),
                    props.getRetention().getMaxTotalSizeMb() * 1024L * 1024L
            );

            log.info("✅ Database backup completed: {}", history.getFileName());

        } catch (Exception e) {
            log.error("❌ Database backup failed", e);
            history.setSuccess(false);
            history.setFailureReason(e.getMessage());
            historyRepo.save(history);
        }

        return DatabaseBackupResultDTO.builder()
                .id(history.getId())
                .fileName(history.getFileName())
                .filePath(history.getFilePath())
                .fileSize(history.getFileSize())
                .encrypted(history.isEncrypted())
                .success(history.isSuccess())
                .failureReason(history.getFailureReason())
                .createdAt(history.getCreatedAt())
                .build();
    }

    public void restoreBackup(UUID backupId, boolean force) throws IOException {
        Path decrypted = null;
        Path sqlFile = null;

        try {
            DatabaseBackupHistory history = historyRepo.findById(backupId)
                    .orElseThrow(() -> new IllegalArgumentException("Backup not found"));

            if (!force) {
                throw new IllegalStateException("Restore requires force=true");
            }

            // ✅ Resolve ONCE from storage root
            Path backupFile = fileStorageService
                    .resolveRelative(history.getFilePath());

            decrypted = encryptionService.decryptIfNeeded(backupFile);
            sqlFile = BackupCompressionUtil.unzip(decrypted);

            DatabaseBackupProperties.Mysql db = props.getMysql();

            ProcessBuilder pb = new ProcessBuilder(
                    db.getBinPath() + "/mysql",
                    "-h", db.getHost(),
                    "-P", String.valueOf(db.getPort()),
                    "-u", db.getUsername(),
                    db.getDatabase()
            );

            pb.redirectInput(sqlFile.toFile());
            pb.redirectError(ProcessBuilder.Redirect.INHERIT);

            int exit = pb.start().waitFor();
            if (exit != 0) {
                throw new IllegalStateException("Restore failed with exit code " + exit);
            }

            // ✅ CLEANUP TEMP FILES
            Files.deleteIfExists(sqlFile);      // extracted .sql
            Files.deleteIfExists(decrypted);    // decrypted .zip

        } catch (Exception e) {
            throw new IllegalStateException("Database restore failed: " + e.getMessage(), e);
        } finally {
            if (sqlFile != null) Files.deleteIfExists(sqlFile);
            if (decrypted != null) Files.deleteIfExists(decrypted);
        }
    }

    public void restoreBackupFromFile(String relativePath, boolean force) {

        if (!force) {
            throw new IllegalStateException("Restore requires force=true");
        }

        try {
            // ---------------------------------------------------------
            // Normalize input: filename OR relative path
            // ---------------------------------------------------------
            String normalizedPath;

            if (relativePath.contains("/") || relativePath.contains("\\")) {
                // Caller provided a relative path (e.g. backups/file.enc)
                normalizedPath = relativePath;
            } else {
                // Caller provided only filename → assume backups dir
                normalizedPath = props.getDir() + "/" + relativePath;
            }

            // ---------------------------------------------------------
            // Resolve safely inside storage root
            // ---------------------------------------------------------
            Path backupFile = fileStorageService.resolveRelative(normalizedPath);

            if (!Files.exists(backupFile)) {
                throw new IllegalArgumentException("Backup file does not exist: " + normalizedPath);
            }

            // ---------------------------------------------------------
            // Decrypt → unzip → restore
            // ---------------------------------------------------------
            Path decrypted = encryptionService.decryptIfNeeded(backupFile);
            Path sqlFile = BackupCompressionUtil.unzip(decrypted);

            DatabaseBackupProperties.Mysql db = props.getMysql();

            ProcessBuilder pb = new ProcessBuilder(
                    db.getBinPath() + "/mysql",
                    "-h", db.getHost(),
                    "-P", String.valueOf(db.getPort()),
                    "-u", db.getUsername(),
                    db.getDatabase()
            );

            pb.redirectInput(sqlFile.toFile());
            pb.redirectError(ProcessBuilder.Redirect.INHERIT);

            int exit = pb.start().waitFor();
            if (exit != 0) {
                throw new IllegalStateException("Restore failed with exit code " + exit);
            }

            // ---------------------------------------------------------
            // Cleanup temp files (leave .enc intact)
            // ---------------------------------------------------------
            Files.deleteIfExists(sqlFile);
            Files.deleteIfExists(decrypted);

        } catch (Exception e) {
            throw new IllegalStateException(
                    "Database restore failed: " + e.getMessage(), e
            );
        }
    }
}