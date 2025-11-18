package com.IntegrityTechnologies.business_manager.config;

import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.util.FileSystemUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.*;
import java.nio.file.attribute.DosFileAttributeView;
import java.util.*;

/**
 * =====================================================================
 * Generic, reusable FileStorageService for multiple modules.
 * ---------------------------------------------------------------------
 * Uses FileStorageProperties for base directories (configurable).
 * =====================================================================
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class FileStorageService {

    private final TransactionalFileManager transactionalFileManager;
    private final FileStorageProperties fileStorageProperties;

    /** Base upload directory (injected from properties). */
    private Path baseUploadDir;

    @PostConstruct
    public void init() {
        baseUploadDir = Paths.get(fileStorageProperties.getBaseUploadDir())
                .toAbsolutePath()
                .normalize();

        try {
            Files.createDirectories(baseUploadDir);
//            log.info("📦 FileStorageService initialized base dir: {}", baseUploadDir);
        } catch (IOException e) {
//            log.error("❌ Failed to initialize upload base directory: {}", e.getMessage(), e);
            throw new RuntimeException("Failed to initialize upload directory", e);
        }
    }

    /**
     * Ensures a directory exists and returns its normalized path.
     */
    public Path initDirectory(Path dir) throws IOException {
        Objects.requireNonNull(dir, "dir must not be null");
        Path normalized = dir.toAbsolutePath().normalize();
        Files.createDirectories(normalized);
//        log.debug("📁 Initialized directory: {}", normalized);
        return normalized;
    }

    /**
     * Returns or creates a module-specific directory, e.g.:
     * module = "supplier", subDir = "supplier-42"
     * => baseUploadDir/supplier/supplier-42
     */
    public Path getModuleDirectory(String module, String subDir) throws IOException {
        Path path = baseUploadDir.resolve(module).resolve(subDir).normalize();
        Files.createDirectories(path);
        return path;
    }

    /**
     * Saves a single file safely from InputStream.
     */
    public Path saveFile(Path dir, String filename, InputStream inputStream) throws IOException {
        Objects.requireNonNull(dir, "dir must not be null");
        Objects.requireNonNull(filename, "filename must not be null");
        Objects.requireNonNull(inputStream, "inputStream must not be null");

        Path normalizedDir = dir.toAbsolutePath().normalize();
        Path target = normalizedDir.resolve(filename).normalize();

        if (!target.startsWith(normalizedDir)) {
            throw new IOException("Invalid file path (path traversal attempt): " + filename);
        }

        Files.createDirectories(normalizedDir);
        try (InputStream in = inputStream) {
            Files.copy(in, target, StandardCopyOption.REPLACE_EXISTING);
        }

//        log.info("💾 Saved file to {}", target);
        return target;
    }

    /**
     * Deletes a single file (ignores missing).
     */
    public void deleteFile(Path file) throws IOException {
        if (file == null) return;
        try {
            Files.deleteIfExists(file);
            // log.info("🗑️ Deleted file {}", file);
        } catch (IOException e) {
            // log.warn("⚠️ Failed to delete file {}", file, e);
            throw e;
        }
    }

    /**
     * Deletes a visible or hidden directory safely (cross-platform).
     */
    public void deleteVisibleOrHiddenDirectory(Path dir) {
        if (dir == null) return;
        try {
            Path parent = dir.getParent();
            if (parent == null) return;

            String name = dir.getFileName().toString();
            Path hiddenDir = parent.resolve("." + name);

            if (Files.exists(hiddenDir)) {
                unhideAndDeleteDirectory(hiddenDir, name);
                return;
            }

            if (Files.exists(dir)) {
                deleteDirectory(dir);
                return;
            }

            // log.debug("No visible or hidden directory found for {}", dir);

        } catch (IOException e) {
            // log.error("Failed to delete directory {}: {}", dir, e.getMessage(), e);
        }
    }

    private void unhideAndDeleteDirectory(Path hiddenDir, String name) throws IOException {
        if (!Files.exists(hiddenDir)) return;
        Path parent = hiddenDir.getParent();
        Path visibleDir = parent.resolve(name);

        try {
            Files.move(hiddenDir, visibleDir, StandardCopyOption.ATOMIC_MOVE);
            // log.debug("Unhid directory {} → {}", hiddenDir, visibleDir);
            deleteDirectory(visibleDir);
        } catch (IOException e) {
            // log.warn("Could not unhide {} (deleting as hidden): {}", hiddenDir, e.getMessage());
            deleteDirectory(hiddenDir);
        }
    }

    public void deleteDirectory(Path dir) throws IOException {
        if (dir == null || !Files.exists(dir)) return;
        Files.walk(dir)
                .sorted(Comparator.reverseOrder())
                .forEach(path -> {
                    try {
                        Files.deleteIfExists(path);
                    } catch (IOException e) {
                        // log.warn("Failed to delete file {}: {}", path, e.getMessage());
                    }
                });
        // log.info("🧹 Deleted directory: {}", dir);
    }

    public void deleteDirectoryAfterCommit(Path dir, String module) {
        transactionalFileManager.runAfterCommit(() -> {
            try {
                if (Files.exists(dir)) {
                    deleteVisibleOrHiddenDirectory(dir);
                    // log.info("🧾 [{}] Directory deleted post-commit: {}", module, dir);
                }
            } catch (Exception e) {
                // log.error("Failed to delete directory {} after commit: {}", dir, e.getMessage(), e);
            }
        });
    }

    /**
     * Resolves a path relative to the configured base upload directory.
     */
    public Path resolveModulePath(String relativePath) {
        Path resolved = baseUploadDir.resolve(relativePath).normalize();
        if (!resolved.startsWith(baseUploadDir)) {
            throw new SecurityException("Attempted path traversal: " + relativePath);
        }
        return resolved;
    }
}