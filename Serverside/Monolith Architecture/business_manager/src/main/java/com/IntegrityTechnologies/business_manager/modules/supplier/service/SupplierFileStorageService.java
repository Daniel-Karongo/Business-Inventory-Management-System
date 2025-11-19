package com.IntegrityTechnologies.business_manager.modules.supplier.service;

import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
import com.IntegrityTechnologies.business_manager.config.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.TransactionalFileManager;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierImage;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import jakarta.annotation.PostConstruct;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.*;
import java.util.*;

/**
 * SupplierFileStorageService
 *
 * Responsibilities:
 *  - Save supplier image files onto disk under: {supplierUploadRoot}/{uploadFolder}/{fileName}
 *  - Provide helper to resolve disk path from uploadFolder + filename
 *  - Create public URL stored in SupplierImage.filePath as:
 *        /api/suppliers/images/{uploadFolder}/{fileName}
 *  - Track saved files with TransactionalFileManager to allow cleanup on rollback
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class SupplierFileStorageService {

    private final FileStorageService fileStorageService;
    private final FileStorageProperties properties;
    private final TransactionalFileManager transactionalFileManager;

    @PostConstruct
    public void init() {
        try {
            Path suppliersUploadDir = Paths.get(properties.getSupplierUploadDir()).toAbsolutePath().normalize();
            // best-effort to prepare or hide dir; FileStorageService may also do its own checks
            hidePathIfSupported(suppliersUploadDir);
        } catch (Exception e) {
            log.debug("Could not initialize supplier upload root: {}", e.getMessage());
        }
    }

    private Path root() {
        return Paths.get(properties.getSupplierUploadDir()).toAbsolutePath().normalize();
    }

    /**
     * Ensure supplier directory exists and return it.
     */
    public Path getSupplierDirectory(String uploadFolder) throws IOException {
        Path supplierDir = root().resolve(uploadFolder).normalize();
        Files.createDirectories(supplierDir);
        hidePathIfSupported(supplierDir);
        return supplierDir;
    }

    /**
     * Store provided multipart files for the supplier.
     * Returns SupplierImage entities with fileName and public filePath.
     *
     * Public URL format set in SupplierImage.filePath:
     *   /api/suppliers/images/{uploadFolder}/{fileName}
     */
    public Set<SupplierImage> storeImages(Supplier supplier, Set<MultipartFile> files) throws IOException {
        if (files == null || files.isEmpty()) return Set.of();

        Path dir = getSupplierDirectory(supplier.getUploadFolder());
        Set<SupplierImage> result = new HashSet<>();

        for (MultipartFile mf : files) {
            if (mf == null || mf.isEmpty()) continue;

            String original = sanitizeOriginalFileName(mf.getOriginalFilename());
            String fileName = UUID.randomUUID() + "_" + System.currentTimeMillis() + "_" + original;

            try (InputStream in = mf.getInputStream()) {
                Path saved = fileStorageService.saveFile(dir, fileName, in);
                transactionalFileManager.track(saved); // for rollback cleanup
                hidePathIfSupported(saved);

                String publicUrl = "/api/suppliers/images/" + supplier.getUploadFolder() + "/" + fileName;

                result.add(SupplierImage.builder()
                        .fileName(fileName)
                        .filePath(publicUrl)
                        .supplier(supplier)
                        .build());
            }
        }
        return result;
    }

    /**
     * Resolve actual disk Path for an image given uploadFolder and filename.
     */
    public Path resolveImagePath(String uploadFolder, String filename) throws IOException {
        Path dir = getSupplierDirectory(uploadFolder);
        return dir.resolve(filename).normalize();
    }

    /**
     * Delete a single file immediately (no transactional deferral).
     */
    public void deleteFile(String uploadFolder, String filename) throws IOException {
        Path pathToDelete = resolveImagePath(uploadFolder, filename);
        fileStorageService.deleteFile(pathToDelete);
    }

    /**
     * Delete a supplier directory immediately.
     */
    public void deleteSupplierDirectory(String uploadDir) throws IOException {
        Path dir = root().resolve(uploadDir).normalize();
        fileStorageService.deleteVisibleOrHiddenDirectory(dir);
    }

    /**
     * Schedule supplier directory deletion after successful DB commit.
     */
    public void deleteSupplierDirectoryAfterCommit(String uploadDir) {
        Path supplierDir = root().resolve(uploadDir).normalize();
        fileStorageService.deleteDirectoryAfterCommit(supplierDir, "supplier");
    }

    /* ====================
       Utilities
       ==================== */

    private String sanitizeOriginalFileName(String original) {
        if (original == null || original.isBlank()) return "file";
        String cleaned = Path.of(original).getFileName().toString();
        return cleaned.replaceAll("[^A-Za-z0-9._-]", "_");
    }

    private void hidePathIfSupported(Path path) {
        try {
            if (System.getProperty("os.name").toLowerCase().contains("win")) {
                if (Files.exists(path)) {
                    Files.setAttribute(path, "dos:hidden", true, LinkOption.NOFOLLOW_LINKS);
                }
            } else {
                Path parent = path.getParent();
                if (parent != null) {
                    String name = path.getFileName().toString();
                    if (!name.startsWith(".")) {
                        Path hiddenPath = parent.resolve("." + name);
                        if (!Files.exists(hiddenPath) && Files.exists(path)) {
                            Files.move(path, hiddenPath, StandardCopyOption.REPLACE_EXISTING);
                        }
                    }
                }
            }
        } catch (IOException e) {
            log.debug("Failed to hide path {}: {}", path, e.getMessage());
        }
    }
}