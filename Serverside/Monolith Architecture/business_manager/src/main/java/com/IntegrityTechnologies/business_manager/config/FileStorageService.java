package com.IntegrityTechnologies.business_manager.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.*;

/**
 * Handles all low-level file storage ops:
 * - Directory creation & cleanup
 * - File saving / deleting
 * - Centralized path normalization
 */
@Service
@Slf4j
public class FileStorageService {

    /**
     * Creates the directory if missing.
     */
    public Path initDirectory(Path dir) throws IOException {
        Files.createDirectories(dir);
        return dir;
    }

    /**
     * Saves an uploaded file to the target directory.
     */
    public Path saveFile(Path dir, String filename, InputStream inputStream) throws IOException {
        Path target = dir.resolve(filename);
        Files.copy(inputStream, target, StandardCopyOption.REPLACE_EXISTING);
        log.info("📁 Saved file: {}", target);
        return target;
    }

    /**
     * Deletes a directory and all contents.
     */
    public void deleteDirectory(Path dir) throws IOException {
        if (!Files.exists(dir)) return;
        try (var paths = Files.walk(dir)) {
            paths.sorted((a, b) -> b.compareTo(a)) // delete children first
                    .forEach(p -> {
                        try {
                            Files.deleteIfExists(p);
                        } catch (IOException e) {
                            log.warn("Failed to delete {}", p, e);
                        }
                    });
        }
        log.info("🗑️ Deleted directory {}", dir);
    }

    /**
     * Deletes a single file.
     */
    public void deleteFile(Path file) throws IOException {
        Files.deleteIfExists(file);
        log.info("🗑️ Deleted file {}", file);
    }
}