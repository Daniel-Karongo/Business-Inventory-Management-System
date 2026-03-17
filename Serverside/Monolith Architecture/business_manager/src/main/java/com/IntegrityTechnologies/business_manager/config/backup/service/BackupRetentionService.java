package com.IntegrityTechnologies.business_manager.common.backup.service;

import com.IntegrityTechnologies.business_manager.config.FileStorageService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class BackupRetentionService {

    private final FileStorageService fileStorageService;

    /**
     * Enforce backup retention directly on the filesystem.
     *
     * @param maxFiles       maximum number of backup files to keep
     * @param maxTotalBytes maximum total size of backups in bytes
     */
    public void enforceRetention(int maxFiles, long maxTotalBytes) {

        try {
            // ---------------------------------------------------------
            // Resolve backups directory (sandboxed)
            // ---------------------------------------------------------
            Path backupsDir = fileStorageService.internalRoot("backups");

            if (!Files.exists(backupsDir)) {
                return;
            }

            // ---------------------------------------------------------
            // Load all encrypted backup files
            // ---------------------------------------------------------
            List<Path> backups = Files.list(backupsDir)
                    .filter(Files::isRegularFile)
                    .filter(p -> p.getFileName().toString().endsWith(".enc"))
                    .sorted(Comparator.comparingLong(this::lastModified))
                    .collect(Collectors.toList());

            long totalSize = backups.stream()
                    .mapToLong(this::size)
                    .sum();

            // ---------------------------------------------------------
            // Enforce retention (oldest-first)
            // ---------------------------------------------------------
            while (backups.size() > maxFiles || totalSize > maxTotalBytes) {

                Path oldest = backups.remove(0);
                long s = size(oldest);

                try {
                    Files.deleteIfExists(oldest);
                    totalSize -= s;
                    log.info("🧹 Deleted old backup {}", oldest.getFileName());
                } catch (Exception e) {
                    log.warn("Failed to delete backup {}", oldest, e);
                    break; // avoid infinite loop
                }
            }

        } catch (Exception e) {
            log.error("Backup retention enforcement failed", e);
        }
    }

    private long size(Path p) {
        try {
            return Files.size(p);
        } catch (Exception e) {
            return 0;
        }
    }

    private long lastModified(Path p) {
        try {
            return Files.getLastModifiedTime(p).toMillis();
        } catch (Exception e) {
            return Long.MAX_VALUE;
        }
    }
}