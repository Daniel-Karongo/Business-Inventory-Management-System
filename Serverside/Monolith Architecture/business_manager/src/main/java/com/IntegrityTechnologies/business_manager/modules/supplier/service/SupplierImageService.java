package com.IntegrityTechnologies.business_manager.modules.supplier.service;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.config.TransactionalFileManager;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.exception.ImageNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.supplier.dto.SupplierDTO;
import com.IntegrityTechnologies.business_manager.modules.supplier.mapper.SupplierMapper;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierImage;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierImageAudit;
import com.IntegrityTechnologies.business_manager.modules.supplier.repository.SupplierImageAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.supplier.repository.SupplierImageRepository;
import com.IntegrityTechnologies.business_manager.modules.supplier.repository.SupplierRepository;
import com.IntegrityTechnologies.business_manager.modules.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

@Service
@RequiredArgsConstructor
@Slf4j
public class SupplierImageService {

    private final TransactionalFileManager transactionalFileManager;
    private final SupplierImageRepository supplierImageRepository;
    private final SupplierRepository supplierRepository;
    private final SupplierFileStorageService supplierFileStorageService;
    private final SupplierImageAuditRepository supplierImageAuditRepository;
    private final SupplierService supplierService;
    private final SupplierMapper supplierMapper;
    private final UserRepository userRepository;

    @Transactional
    public SupplierDTO updateSupplierImages(UUID id, Set<MultipartFile> newImages, String updaterUsername) throws IOException {
        Supplier existing = supplierService.getSupplier(id);

        if (newImages != null && !newImages.isEmpty()) {
            Set<SupplierImage> saved = supplierFileStorageService.storeImages(existing, newImages);
            supplierImageRepository.saveAll(saved);
            existing.getImages().addAll(saved);
            for (SupplierImage im : saved) {
                logSupplierImageAudit(im, "Supplier's images updated", "UPLOADED", existing);
            }
            supplierService.logSupplierAuditAsync(existing, "UPDATED", "images", null, "Added new images", "Supplier image(s) added");
        }

        if (updaterUsername != null) {
            userRepository.findByUsername(updaterUsername)
                    .ifPresent(existing::setUpdatedBy);
        }
        existing.setUpdatedAt(LocalDateTime.now());
        Supplier saved = supplierRepository.save(existing);
        return supplierMapper.toDTO(saved);
    }

    public List<String> getAllSuppliersImages(Boolean deletedSupplier, Boolean deletedImage) {
        List<SupplierImage> images = supplierImageRepository.findAll().stream()
                .filter(img -> {
                    Supplier supplier = img.getSupplier();
                    boolean keepSupplier = deletedSupplier == null
                            ? true
                            : deletedSupplier
                            ? Boolean.TRUE.equals(supplier.getDeleted())
                            : !Boolean.TRUE.equals(supplier.getDeleted());

                    boolean keepImage = deletedImage == null
                            ? true
                            : deletedImage
                            ? Boolean.TRUE.equals(img.getDeleted())
                            : !Boolean.TRUE.equals(img.getDeleted());

                    return keepSupplier && keepImage;
                })
                .toList();

        if (images.isEmpty()) {
            throw new ImageNotFoundException("No supplier images found matching criteria");
        }

        // Audit
        logSupplierImageAudit(null, "Retrieved list of supplier images", "RETRIEVE", null);

        return images.stream()
                .map(SupplierImage::getFilePath)
                .toList();
    }

    public ResponseEntity<Resource> downloadAllSuppliersImages(Boolean deletedSupplier, Boolean deletedImage) throws IOException {
        List<SupplierImage> images = supplierImageRepository.findAll().stream()
                .filter(img -> {
                    Supplier supplier = img.getSupplier();
                    boolean keepSupplier = deletedSupplier == null
                            ? true
                            : deletedSupplier
                            ? Boolean.TRUE.equals(supplier.getDeleted())
                            : !Boolean.TRUE.equals(supplier.getDeleted());

                    boolean keepImage = deletedImage == null
                            ? true
                            : deletedImage
                            ? Boolean.TRUE.equals(img.getDeleted())
                            : !Boolean.TRUE.equals(img.getDeleted());

                    return keepSupplier && keepImage;
                })
                .toList();

        if (images.isEmpty()) {
            throw new ImageNotFoundException("No supplier images found matching criteria");
        }

        // Create temp ZIP
        File tempZip = File.createTempFile("supplier-images-", ".zip");

        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(tempZip))) {
            for (SupplierImage img : images) {
                Supplier supplier = img.getSupplier();
                Path imgPath = supplierFileStorageService.resolveImagePath(supplier.getUploadFolder(), img.getFileName());

                if (!Files.exists(imgPath) || !Files.isRegularFile(imgPath)) continue;

                String entryName = supplier.getId() + "/" + imgPath.getFileName();
                zos.putNextEntry(new ZipEntry(entryName));
                Files.copy(imgPath, zos);
                zos.closeEntry();
            }
            zos.finish();
        }

        // Audit ZIP download
        logSupplierImageAudit(null, "All suppliers' images downloaded as ZIP", "ZIP_DOWNLOADED", null);

        InputStreamResource resource = new InputStreamResource(new FileInputStream(tempZip));

        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"supplier-images.zip\"")
                .body(resource);
    }

    /**
     * Returns the public URLs stored in SupplierImage.filePath (already in the desired format)
     */
    public List<String> getSupplierImageUrls(UUID supplierId, Boolean deleted) {

        List<SupplierImage> images;

        if (deleted == null) {
            images = supplierImageRepository.findBySupplierId(supplierId);
        } else if (deleted) {
            images = supplierImageRepository.findBySupplierIdAndDeletedTrue(supplierId);
        } else {
            images = supplierImageRepository.findBySupplierIdAndDeletedFalse(supplierId);
        }

        return images.stream()
                .map(SupplierImage::getFilePath)
                .toList();
    }


    /**
     * Download a single image by supplierId + filename.
     * The SupplierImage.filePath contains the public URL only; resolve disk path via uploadFolder.
     */
    public Resource downloadImage(UUID supplierId, String filename, Boolean deleted) throws IOException {
        SupplierImage image = getImage(supplierId, filename, deleted);
        Path path = resolveImagePath(supplierId, filename);
        return loadImageAsResource(path);
    }

    public SupplierImage getImage(UUID supplierId, String filename, Boolean deleted) {
        if (deleted == null) {
            return supplierImageRepository.findBySupplierIdAndFileName(supplierId, filename)
                    .orElseThrow(() -> new EntityNotFoundException("Image not found"));
        }
        return deleted
                ? supplierImageRepository.findBySupplierIdAndFileNameAndDeletedTrue(supplierId, filename)
                .orElseThrow(() -> new EntityNotFoundException("Deleted image not found"))
                : supplierImageRepository.findBySupplierIdAndFileNameAndDeletedFalse(supplierId, filename)
                .orElseThrow(() -> new EntityNotFoundException("Active image not found"));
    }

    public Path resolveImagePath(UUID supplierId, String filename) throws IOException {
        Supplier supplier = supplierRepository.findById(supplierId)
                .orElseThrow(() -> new EntityNotFoundException("Supplier not found"));

        return supplierFileStorageService.resolveImagePath(supplier.getUploadFolder(), filename);
    }

    public Resource loadImageAsResource(Path path) {
        if (!Files.exists(path))
            throw new EntityNotFoundException("Image file not found on disk");

        return new FileSystemResource(path.toFile());
    }


    /**
     * Stream zip of supplier images using the supplier.uploadFolder to resolve disk paths.
     */
    public List<SupplierImage> getImages(UUID supplierId, Boolean deleted) {
        List<SupplierImage> images = deleted == null
                ? supplierImageRepository.findBySupplierId(supplierId)
                : deleted
                ? supplierImageRepository.findBySupplierIdAndDeletedTrue(supplierId)
                : supplierImageRepository.findBySupplierIdAndDeletedFalse(supplierId);

        if (images.isEmpty())
            throw new ImageNotFoundException("No images found for supplier");

        return images;
    }

    public Supplier getSupplier(UUID supplierId) {
        return supplierRepository.findById(supplierId)
                .orElseThrow(() -> new EntityNotFoundException("Supplier not found"));
    }

    public void writeImagesToZip(List<SupplierImage> images, Supplier supplier, OutputStream out) throws IOException {
        try (ZipOutputStream zos = new ZipOutputStream(out)) {
            for (SupplierImage img : images) {
                Path imgPath = supplierFileStorageService.resolveImagePath(
                        supplier.getUploadFolder(), img.getFileName());

                if (!Files.exists(imgPath)) continue;

                zos.putNextEntry(new ZipEntry(img.getFileName()));
                Files.copy(imgPath, zos);
                zos.closeEntry();
            }
        }
        logSupplierImageAudit(null, "All supplier images downloaded as ZIP", "ZIP_DOWNLOADED", supplier);
    }

    @Transactional
    public ResponseEntity<ApiResponse> softDeleteSupplierImage(UUID supplierId, String filename) throws IOException {
        SupplierImage image = supplierImageRepository
                .findBySupplierIdAndFileNameAndDeletedFalse(supplierId, filename)
                .orElseThrow(() -> new ImageNotFoundException("Active image not found: " + filename));

        Supplier supplier = supplierRepository.findById(supplierId)
                .orElseThrow(() -> new ImageNotFoundException("Supplier not found: " + supplierId));

        // Soft-delete
        image.setDeleted(true);
        image.setDeletedAt(LocalDateTime.now());
        supplierImageRepository.save(image);

        // Audit
        logSupplierImageAudit(image, "Manual deletion", "SOFT_DELETED", supplier);

        return ResponseEntity.ok(new ApiResponse("success", "Image soft-deleted successfully"));
    }

    @Transactional
    public ResponseEntity<ApiResponse> softDeleteSupplierImagesBulk(UUID supplierId, List<String> filenames) throws IOException {
        if (filenames == null || filenames.isEmpty())
            throw new IllegalArgumentException("No filenames provided for deletion");

        List<SupplierImage> images = supplierImageRepository.findBySupplierIdAndDeletedFalse(supplierId);

        List<SupplierImage> toDelete = images.stream()
                .filter(i -> filenames.contains(i.getFileName()))
                .collect(Collectors.toList());

        if (toDelete.isEmpty())
            throw new ImageNotFoundException("No matching active images found");

        Supplier supplier = supplierRepository.findById(supplierId)
                .orElseThrow(() -> new ImageNotFoundException("Supplier not found: " + supplierId));

        // Soft delete records
        toDelete.forEach(img -> {
            img.setDeleted(true);
            img.setDeletedAt(LocalDateTime.now());
            supplierImageRepository.save(img);

            logSupplierImageAudit(img, "Batch deletion", "SOFT_DELETED", supplier);
        });

        return ResponseEntity.ok(new ApiResponse("success", "Images soft deleted successfully"));
    }

    @Transactional
    public ResponseEntity<ApiResponse> deleteSupplierImage(UUID supplierId, String filename) throws IOException {
        SupplierImage image = supplierImageRepository.findBySupplierIdAndFileName(supplierId, filename)
                .orElseThrow(() -> new ImageNotFoundException("Image not found: " + filename));

        Supplier supplier = supplierRepository.findById(supplierId)
                .orElseThrow(() -> new EntityNotFoundException("Supplier not found: " + supplierId));

        // Delete DB record
        supplierImageRepository.delete(image);

        // Delete file on disk
        try {
            supplierFileStorageService.deleteFile(supplier.getUploadFolder(), filename);
        } catch (IOException e) {
            log.warn("Failed to delete file {} for supplier {}: {}", filename, supplierId, e.getMessage());
        }

        // Audit
        logSupplierImageAudit(image, "Manual deletion", "DELETED", supplier);

        return ResponseEntity.ok(new ApiResponse("success", "Image deleted successfully"));
    }

    @Transactional
    public ResponseEntity<ApiResponse> deleteSupplierImagesBulk(UUID supplierId, List<String> filenames) throws IOException {
        if (filenames == null || filenames.isEmpty()) throw new IllegalArgumentException("No filenames provided for deletion");
        List<SupplierImage> images = supplierImageRepository.findBySupplierIdAndDeletedFalse(supplierId);
        List<SupplierImage> toDelete = images.stream().filter(i -> filenames.contains(i.getFileName())).collect(Collectors.toList());
        if (toDelete.isEmpty()) throw new ImageNotFoundException("No matching images found for deletion");

        Supplier supplier = supplierRepository.findById(supplierId)
                .orElseThrow(() -> new EntityNotFoundException("Supplier not found: " + supplierId));

        // Batch delete DB records
        supplierImageRepository.deleteAll(toDelete);

        for (SupplierImage image : toDelete) {
            try {
                supplierFileStorageService.deleteFile(supplier.getUploadFolder(), image.getFileName());
            } catch (IOException e) {
                log.warn("File deletion failed: {} for supplier {}: {}", image.getFileName(), supplierId, e.getMessage());
            }

            logSupplierImageAudit(image, "Batch deletion", "DELETED", supplier);
        }
        log.info("Deleted {} images for supplier {}", toDelete.size(), supplierId);
        ApiResponse response = new ApiResponse("success", "Images deleted successfully");
        return ResponseEntity.ok(response);
    }

    @Transactional
    public ResponseEntity<ApiResponse> restoreSupplierImage(UUID supplierId, String filename) {

        // Find the soft-deleted image
        SupplierImage image = supplierImageRepository
                .findBySupplierIdAndFileNameAndDeletedTrue(supplierId, filename)
                .orElseThrow(() -> new ImageNotFoundException("Soft deleted image not found"));

        // Restore
        image.setDeleted(false);
        image.setDeletedAt(null);
        supplierImageRepository.save(image);

        // Get supplier name directly (no unnecessary SupplierDTO mapping)
        Supplier supplier = supplierRepository.findById(supplierId).orElse(null);

        // Audit record
        if(supplier != null) {
            logSupplierImageAudit(image, "Image Restored", "RESTORE", supplier);

            return ResponseEntity.ok(
                    new ApiResponse("success", "Image restored successfully")
            );
        } else {
            return ResponseEntity.ok(new ApiResponse("Failed", "No such supplier found"));
        }
    }

    @Transactional
    public ResponseEntity<ApiResponse> restoreSupplierImagesBulk(UUID supplierId, List<String> filenames) {
        List<SupplierImage> toRestore = supplierImageRepository.findBySupplierIdAndDeletedTrue(supplierId)
                .stream().filter(i -> filenames.contains(i.getFileName()))
                .toList();

        if (toRestore.isEmpty())
            throw new ImageNotFoundException("No soft-deleted images found");

        Supplier supplier = supplierRepository.findById(supplierId).orElse(null);

        if(supplier != null) {
            toRestore.forEach(img -> {
                img.setDeleted(false);
                img.setDeletedAt(null);
                supplierImageRepository.save(img);

                logSupplierImageAudit(img, "Image Restored", "RESTORE", supplier);
            });
            return ResponseEntity.ok(new ApiResponse("success", "Images restored successfully"));
        } else {
            return ResponseEntity.ok(new ApiResponse("Failed", "No such supplier found"));
        }
    }


    @Transactional
    public ResponseEntity<List<SupplierImageAudit>> getIndividualSupplierImageAudit(String identifier, Boolean deleted) {
        SupplierDTO supplier = supplierService.getByIdentifier(identifier, deleted);
        return ResponseEntity.ok(supplierImageAuditRepository.findBySupplierIdOrderByTimestampDesc(supplier.getId()));
    }

    @Transactional
    public ResponseEntity<List<SupplierImageAudit>> getAllSuppliersImagesAudits() {
        return ResponseEntity.ok(supplierImageAuditRepository.findAllByOrderByTimestampDesc());
    }

    private void logSupplierImageAudit(SupplierImage image, String reason, String action, Supplier supplier) {
        supplierImageAuditRepository.save(
                SupplierImageAudit.builder()
                        .fileName(image != null ? image.getFileName() : "ALL")
                        .filePath(image.getFilePath())
                        .action(action)
                        .reason(reason)
                        .timestamp(LocalDateTime.now())
                        .supplierId(supplier != null ? supplier.getId() : null)
                        .supplierName(supplier != null ? supplier.getName() : "ALL SUPPLIERS")
                        .performedBy(SecurityUtils.currentUsername())
                        .build()
        );
    }
}