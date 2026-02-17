package com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.service;

import com.IntegrityTechnologies.business_manager.common.FIleUploadDTO;
import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
import com.IntegrityTechnologies.business_manager.config.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.TransactionalFileManager;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.SupplierImage;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

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
    private final TransactionalFileManager transactionalFileManager;

    private Path root() {
        return fileStorageService.supplierRoot();
    }

    /**
     * Ensure supplier directory exists and return it.
     */
    public Path getSupplierDirectory(String uploadFolder) throws IOException {
        return fileStorageService.initDirectory(
                root().resolve(uploadFolder)
        );
    }

    /**
     * Store provided multipart files for the supplier.
     * Returns SupplierImage entities with fileName and public filePath.
     *
     * Public URL format set in SupplierImage.filePath:
     *   /api/suppliers/images/{uploadFolder}/{fileName}
     */
    public Set<SupplierImage> storeImages(Supplier supplier, List<FIleUploadDTO> files) throws IOException {
        if (files == null || files.isEmpty()) return Set.of();

        Path dir = getSupplierDirectory(supplier.getUploadFolder());
        Set<SupplierImage> result = new HashSet<>();

        for (FIleUploadDTO fileDTO : files) {
            if (fileDTO == null || fileDTO.getFile().isEmpty()) continue;

            MultipartFile file = fileDTO.getFile();

            String original = sanitizeOriginalFileName(file.getOriginalFilename());
            String fileName = UUID.randomUUID() + "_" + System.currentTimeMillis() + "_" + original;

            try (InputStream in = file.getInputStream()) {
                Path saved = fileStorageService.saveFile(dir, fileName, in);
                transactionalFileManager.track(saved);

                String publicUrl = "/api/suppliers/images/" + supplier.getUploadFolder() + "/" + fileName;

                result.add(SupplierImage.builder()
                        .fileName(fileName)
                        .filePath(publicUrl)
                        .fileDescription(fileDTO.getDescription())
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

    private String sanitizeOriginalFileName(String original) {
        if (original == null || original.isBlank()) return "file";
        String cleaned = Path.of(original).getFileName().toString();
        return cleaned.replaceAll("[^A-Za-z0-9._-]", "_");
    }
}