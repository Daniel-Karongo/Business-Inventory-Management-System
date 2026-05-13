package com.IntegrityTechnologies.business_manager.modules.person.supplier.service;

import com.IntegrityTechnologies.business_manager.config.files.FIleUploadDTO;
import com.IntegrityTechnologies.business_manager.config.files.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.files.TransactionalFileManager;
import com.IntegrityTechnologies.business_manager.modules.person.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.supplier.model.SupplierImage;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

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

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    private Path root(UUID branchId) {
        return fileStorageService.supplierRoot(branchId);
    }

    /**
     * Ensure supplier directory exists and return it.
     */
    public Path getSupplierDirectory(UUID branchId, String uploadFolder) throws IOException {

        String safeFolder = uploadFolder.replaceAll("[^A-Za-z0-9._-]", "_");

        Path dir = root(branchId).resolve(safeFolder).normalize();

        if (!dir.startsWith(root(branchId))) {
            throw new SecurityException("Invalid upload folder");
        }

        return fileStorageService.initDirectory(dir);
    }

    /**
     * Store provided multipart files for the supplier.
     * Returns SupplierImage entities with fileName and public filePath.
     *
     * Public URL format set in SupplierImage.filePath:
     *   /api/suppliers/images/{uploadFolder}/{fileName}
     */
    public Set<SupplierImage> storeImages(UUID branchId, Supplier supplier, List<FIleUploadDTO> files) throws IOException {
        if (files == null || files.isEmpty()) return Set.of();

        Path dir = getSupplierDirectory(branchId, supplier.getUploadFolder());
        Set<SupplierImage> result = new HashSet<>();

        for (FIleUploadDTO fileDTO : files) {
            if (fileDTO == null || fileDTO.getFile().isEmpty()) continue;

            MultipartFile file = fileDTO.getFile();

            String original = sanitizeOriginalFileName(file.getOriginalFilename());
            String fileName = UUID.randomUUID() + "_" + original.substring(0, Math.min(original.length(), 50));

            try (InputStream in = file.getInputStream()) {
                Path saved = fileStorageService.saveFile(dir, fileName, in);
                transactionalFileManager.track(saved);

                String publicUrl = "/api/suppliers/images/"
                        + TenantContext.getTenantId() + "/"
                        + supplier.getBranchId() + "/"
                        + supplier.getUploadFolder() + "/"
                        + fileName;

                result.add(SupplierImage.builder()
                        .fileName(fileName)
                        .filePath(publicUrl)
                        .fileDescription(fileDTO.getDescription())
                        .supplier(supplier)
                        .tenantId(tenantId())
                        .branchId(branchId)
                        .build());
            }
        }
        return result;
    }

    /**
     * Resolve actual disk Path for an image given uploadFolder and filename.
     */
    public Path resolveImagePath(UUID branchId, String uploadFolder, String filename) throws IOException {

        String safeFile = filename.replaceAll("[^A-Za-z0-9._-]", "_");

        Path dir = getSupplierDirectory(branchId, uploadFolder);
        Path resolved = dir.resolve(safeFile).normalize();

        if (!resolved.startsWith(dir)) {
            throw new SecurityException("Invalid file path");
        }

        return resolved;
    }

    /**
     * Delete a single file immediately (no transactional deferral).
     */
    public void deleteFile(UUID branchId, String uploadFolder, String filename) throws IOException {
        Path pathToDelete = resolveImagePath(branchId, uploadFolder, filename);
        fileStorageService.deleteFile(pathToDelete);
    }

    private String sanitizeOriginalFileName(String original) {
        if (original == null || original.isBlank()) return "file";
        String cleaned = Path.of(original).getFileName().toString();
        return cleaned.replaceAll("[^A-Za-z0-9._-]", "_");
    }
}