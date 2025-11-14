package com.IntegrityTechnologies.business_manager.modules.supplier.service;

import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
import com.IntegrityTechnologies.business_manager.config.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.TransactionalFileManager;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierImage;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.*;
import java.util.*;

/**
 * =====================================================================
 * SupplierFileStorageService
 * ---------------------------------------------------------------------
 * Handles supplier-specific file operations, delegating low-level file
 * logic to FileStorageService for consistency and reusability.
 *
 * Features:
 *  - Each supplier gets its own directory under supplier uploads
 *  - Files saved transactionally, rolled back on failure
 *  - Hidden/visible directory cleanup (cross-platform)
 * =====================================================================
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class SupplierFileStorageService {

    private final FileStorageService fileStorageService;
    private final FileStorageProperties properties;
    private final TransactionalFileManager transactionalFileManager;

    /**
     * Root path for supplier uploads.
     */
    @PostConstruct
    public void init() {
        try {
            Path suppliersUploadDir = Paths.get(properties.getSupplierUploadDir()).toAbsolutePath().normalize();
            hidePathIfSupported(suppliersUploadDir);
            log.debug("📁 Verified and prepared supplier root directory: {}", suppliersUploadDir);
        } catch (Exception e) {
            log.warn("⚠️ Could not prepare supplier root directory: {}", e.getMessage());
        }
    }

    private Path root() {
        return Paths.get(properties.getSupplierUploadDir()).toAbsolutePath().normalize();
    }

    /**
     * Ensures supplier directory exists (hidden if possible).
     */
    public Path getSupplierDirectory(UUID supplierId) throws IOException {
        Path baseDir = root();
        Path supplierDir = baseDir.resolve("supplier-" + supplierId).normalize();
        Files.createDirectories(supplierDir);


        hidePathIfSupported(supplierDir);
        log.debug("📁 Supplier directory initialized: {}", supplierDir);
        return supplierDir;
    }

    /**
     * Stores supplier images and returns a list of SupplierImage entities.
     * Automatically tracks files for rollback.
     */
    public List<SupplierImage> storeImages(Supplier supplier, List<MultipartFile> files) throws IOException {
        if (files == null || files.isEmpty()) return List.of();

        Path dir = getSupplierDirectory(supplier.getId());
        List<SupplierImage> result = new ArrayList<>();

        for (MultipartFile mf : files) {
            if (mf.isEmpty()) continue;

            String original = sanitizeOriginalFileName(mf.getOriginalFilename());
            String filename = UUID.randomUUID() + "_" + System.currentTimeMillis() + "_" + original;

            try (InputStream in = mf.getInputStream()) {
                Path saved = fileStorageService.saveFile(dir, filename, in);
                transactionalFileManager.track(saved); // rollback safety
                hidePathIfSupported(saved);

                result.add(SupplierImage.builder()
                        .fileName(filename)
                        .filePath(saved.toString())
                        .supplier(supplier)
                        .build());

                log.debug("✅ Saved supplier image: {}", filename);
            }
        }
        return result;
    }

    /**
     * Deletes all files belonging to a supplier.
     * Uses transactional safety — deletes only after successful commit.
     */
    public void deleteSupplierDirectoryAfterCommit(Long supplierId) {
        Path supplierDir = root().resolve("supplier-" + supplierId).normalize();
        fileStorageService.deleteDirectoryAfterCommit(supplierDir, "supplier");
    }

    /**
     * Immediate deletion (no transaction deferral).
     */
    public void deleteSupplierDirectory(Long supplierId) throws IOException {
        Path dir = root().resolve("supplier-" + supplierId).normalize();
        fileStorageService.deleteVisibleOrHiddenDirectory(dir);
    }

    /**
     * Deletes a single file (directly).
     */
    public void deleteFile(Path path) throws IOException {
        fileStorageService.deleteFile(path);
    }

    /**
     * Cleans up hidden or visible supplier directories (manual maintenance).
     */
    public void cleanupSupplierDirectory(Long supplierId) throws IOException {
        Path baseDir = root();
        Path visible = baseDir.resolve("supplier-" + supplierId);
        Path hidden = baseDir.resolve(".supplier-" + supplierId);

        if (Files.exists(hidden)) {
            log.debug("🧹 Cleaning hidden supplier directory: {}", hidden);
            fileStorageService.deleteVisibleOrHiddenDirectory(hidden);
        } else if (Files.exists(visible)) {
            log.debug("🧹 Cleaning visible supplier directory: {}", visible);
            fileStorageService.deleteVisibleOrHiddenDirectory(visible);
        } else {
            log.info("ℹ️ No directory found for supplier {}", supplierId);
        }
    }

    /* ================================================================
       UTILITIES
       ================================================================ */

    private String sanitizeOriginalFileName(String original) {
        if (original == null || original.isBlank()) return "file";
        String cleaned = Path.of(original).getFileName().toString();
        return cleaned.replaceAll("[^A-Za-z0-9._-]", "_");
    }

    private void hidePathIfSupported(Path path) {
        try {
            if (System.getProperty("os.name").toLowerCase().contains("win")) {
                Files.setAttribute(path, "dos:hidden", true, LinkOption.NOFOLLOW_LINKS);
            } else {
                Path parent = path.getParent();
                if (parent != null) {
                    String name = path.getFileName().toString();
                    if (!name.startsWith(".")) {
                        Path hiddenPath = parent.resolve("." + name);
                        if (!Files.exists(hiddenPath)) {
                            Files.move(path, hiddenPath, StandardCopyOption.REPLACE_EXISTING);
                            log.debug("👻 Renamed {} → {}", name, hiddenPath.getFileName());
                        }
                    }
                }
            }
        } catch (IOException e) {
            log.warn("⚠️ Could not hide path {}: {}", path, e.getMessage());
        }
    }
}