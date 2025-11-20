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
                supplierImageAuditRepository.save(SupplierImageAudit.builder()
                        .fileName(im.getFileName())
                        .filePath(im.getFilePath())
                        .action("UPLOADED")
                        .timestamp(LocalDateTime.now())
                        .supplierId(id)
                        .supplierName(existing.getName())
                        .performedBy(updaterUsername)
                        .build());
            }
            supplierService.logSupplierAudit(existing, "UPDATED", "images", null, "Added new images", "Supplier image(s) added");
        }

        if (updaterUsername != null) {
            userRepository.findByUsername(updaterUsername)
                    .ifPresent(existing::setUpdatedBy);
        }
        existing.setUpdatedAt(LocalDateTime.now());
        Supplier saved = supplierRepository.save(existing);
        return supplierMapper.toDTO(saved);
    }

    public List<String> getAllSuppliersImages(Boolean deleted) {
        List<SupplierImage> images;

        if (deleted == null) {
            images = supplierImageRepository.findAll();
        } else if (deleted) {
            images = supplierImageRepository.findAll().stream()
                    .filter(SupplierImage::getDeleted)
                    .toList();
        } else {
            images = supplierImageRepository.findAll().stream()
                    .filter(i -> !Boolean.TRUE.equals(i.getDeleted()))
                    .toList();
        }

        if (images.isEmpty()) {
            throw new ImageNotFoundException("No supplier images found matching deleted=" + deleted);
        }

        // Audit
        supplierImageAuditRepository.save(SupplierImageAudit.builder()
                .fileName("ALL")
                .action("RETRIEVE")
                .reason("Retrieved list of supplier images")
                .timestamp(LocalDateTime.now())
                .performedBy(supplierService.currentUser())
                .build());

        return images.stream()
                .map(SupplierImage::getFilePath)
                .toList();
    }

    public ResponseEntity<Resource> downloadAllSuppliersImages(Boolean deleted) throws IOException {
        List<SupplierImage> images;

        if (deleted == null) {
            images = supplierImageRepository.findAll();
        } else if (deleted) {
            images = supplierImageRepository.findAll().stream()
                    .filter(SupplierImage::getDeleted)
                    .toList();
        } else {
            images = supplierImageRepository.findAll().stream()
                    .filter(i -> !Boolean.TRUE.equals(i.getDeleted()))
                    .toList();
        }

        if (images.isEmpty()) {
            throw new ImageNotFoundException("No supplier images found matching deleted=" + deleted);
        }

        // Create temp ZIP
        File tempZip = File.createTempFile("supplier-images-", ".zip");

        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(tempZip))) {
            for (SupplierImage img : images) {
                Supplier supplier = img.getSupplier();
                Path imgPath = supplierFileStorageService.resolveImagePath(supplier.getUploadFolder(), img.getFileName());

                if (!Files.exists(imgPath) || !Files.isRegularFile(imgPath)) continue;

                zos.putNextEntry(new ZipEntry(img.getFileName()));
                Files.copy(imgPath, zos);
                zos.closeEntry();
            }
            zos.finish();
        }

        // Audit ZIP download
        supplierImageAuditRepository.save(SupplierImageAudit.builder()
                .fileName("ALL")
                .action("ZIP_DOWNLOADED")
                .reason("All supplier images downloaded as ZIP")
                .timestamp(LocalDateTime.now())
                .performedBy(supplierService.currentUser())
                .build());

        InputStreamResource resource = new InputStreamResource(new FileInputStream(tempZip));

        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"supplier-images.zip\"")
                .body(resource);
    }

    /**
     * Returns the public URLs stored in SupplierImage.filePath (already in the desired format)
     */
    public List<String> getSupplierImageUrls(String supplierId, Boolean deleted) {

        List<SupplierImage> images;

        if (deleted == null) {
            images = supplierImageRepository.findBySupplierId(UUID.fromString(supplierId));
        } else if (deleted) {
            images = supplierImageRepository.findBySupplierIdAndDeletedTrue(UUID.fromString(supplierId));
        } else {
            images = supplierImageRepository.findBySupplierIdAndDeletedFalse(UUID.fromString(supplierId));
        }

        return images.stream()
                .map(SupplierImage::getFilePath)
                .toList();
    }


    /**
     * Download a single image by supplierId + filename.
     * The SupplierImage.filePath contains the public URL only; resolve disk path via uploadFolder.
     */
    public Resource downloadImage(String supplierId, String filename, Boolean deleted) throws IOException {

        SupplierImage image;

        if (deleted == null) {
            image = supplierImageRepository.findBySupplierIdAndFileName(UUID.fromString(supplierId), filename)
                    .orElseThrow(() -> new EntityNotFoundException("Image not found"));
        } else if (deleted) {
            image = supplierImageRepository.findBySupplierIdAndFileNameAndDeletedTrue(UUID.fromString(supplierId), filename)
                    .orElseThrow(() -> new EntityNotFoundException("Deleted image not found"));
        } else {
            image = supplierImageRepository.findBySupplierIdAndFileNameAndDeletedFalse(UUID.fromString(supplierId), filename)
                    .orElseThrow(() -> new EntityNotFoundException("Active image not found"));
        }

        Supplier supplier = supplierRepository.findById(UUID.fromString(supplierId))
                .orElseThrow(() -> new EntityNotFoundException("Supplier not found"));

        Path path = supplierFileStorageService.resolveImagePath(supplier.getUploadFolder(), filename);

        if (!Files.exists(path)) throw new EntityNotFoundException("Image file not found on disk");

        return new FileSystemResource(path.toFile());
    }

    /**
     * Stream zip of supplier images using the supplier.uploadFolder to resolve disk paths.
     */
    public void streamAllImagesAsZip(String supplierId, OutputStream out, Boolean deleted) throws IOException {

        List<SupplierImage> images;

        if (deleted == null) {
            images = supplierImageRepository.findBySupplierId(UUID.fromString(supplierId));
        } else if (deleted) {
            images = supplierImageRepository.findBySupplierIdAndDeletedTrue(UUID.fromString(supplierId));
        } else {
            images = supplierImageRepository.findBySupplierIdAndDeletedFalse(UUID.fromString(supplierId));
        }

        if (images.isEmpty())
            throw new EntityNotFoundException("No images found for supplier");

        Supplier supplier = supplierRepository.findById(UUID.fromString(supplierId))
                .orElseThrow(() -> new EntityNotFoundException("Supplier not found"));

        try (ZipOutputStream zos = new ZipOutputStream(out)) {
            for (SupplierImage img : images) {
                Path imgPath = supplierFileStorageService.resolveImagePath(supplier.getUploadFolder(), img.getFileName());

                if (!Files.exists(imgPath)) continue;

                zos.putNextEntry(new ZipEntry(img.getFileName()));
                Files.copy(imgPath, zos);
                zos.closeEntry();
            }
            zos.finish();
        }

        supplierImageAuditRepository.save(SupplierImageAudit.builder()
                .fileName("ALL")
                .action("ZIP_DOWNLOADED")
                .reason("All supplier images downloaded as ZIP")
                .timestamp(LocalDateTime.now())
                .supplierId(UUID.fromString(supplierId))
                .supplierName(supplierRepository.findById(UUID.fromString(supplierId)).map(Supplier::getName).orElse(null))
                .performedBy(supplierService.currentUser())
                .build());
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
        supplierImageAuditRepository.save(SupplierImageAudit.builder()
                .fileName(image.getFileName())
                .filePath(image.getFilePath())
                .action("SOFT_DELETED")
                .reason("Manual deletion")
                .timestamp(LocalDateTime.now())
                .supplierId(supplierId)
                .supplierName(supplier.getName())
                .performedBy(supplierService.currentUser())
                .build());

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

            supplierImageAuditRepository.save(SupplierImageAudit.builder()
                    .fileName(img.getFileName())
                    .filePath(img.getFilePath())
                    .action("SOFT_DELETED")
                    .reason("Batch deletion")
                    .timestamp(LocalDateTime.now())
                    .supplierId(supplierId)
                    .supplierName(supplier.getName())
                    .performedBy(supplierService.currentUser())
                    .build());
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
        supplierImageAuditRepository.save(SupplierImageAudit.builder()
                .fileName(image.getFileName())
                .filePath(image.getFilePath())
                .action("DELETED")
                .reason("Manual deletion")
                .timestamp(LocalDateTime.now())
                .supplierId(supplierId)
                .supplierName(supplier.getName())
                .performedBy(supplierService.currentUser())
                .build());

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

            supplierImageAuditRepository.save(SupplierImageAudit.builder()
                    .fileName(image.getFileName())
                    .filePath(image.getFilePath())
                    .action("DELETED")
                    .reason("Batch deletion")
                    .timestamp(LocalDateTime.now())
                    .supplierId(supplierId)
                    .supplierName(supplier.getName())
                    .performedBy(supplierService.currentUser())
                    .build());
        }
        log.info("Deleted {} images for supplier {}", toDelete.size(), supplierId);
        ApiResponse response = new ApiResponse("success", "Images deleted successfully");
        return ResponseEntity.ok(response);
    }

    @Transactional
    public ResponseEntity<ApiResponse> restoreSupplierImage(UUID supplierId, String filename) {
        SupplierImage image = supplierImageRepository
                .findBySupplierIdAndFileNameAndDeletedTrue(supplierId, filename)
                .orElseThrow(() -> new ImageNotFoundException("Soft deleted image not found"));

        image.setDeleted(false);
        image.setDeletedAt(null);
        supplierImageRepository.save(image);

        return ResponseEntity.ok(new ApiResponse("success", "Image restored successfully"));
    }

    @Transactional
    public ResponseEntity<ApiResponse> restoreSupplierImagesBulk(UUID supplierId, List<String> filenames) {
        List<SupplierImage> toRestore = supplierImageRepository.findBySupplierIdAndDeletedTrue(supplierId)
                .stream().filter(i -> filenames.contains(i.getFileName()))
                .toList();

        if (toRestore.isEmpty())
            throw new ImageNotFoundException("No soft-deleted images found");

        toRestore.forEach(img -> {
            img.setDeleted(false);
            img.setDeletedAt(null);
            supplierImageRepository.save(img);
        });

        return ResponseEntity.ok(new ApiResponse("success", "Images restored successfully"));
    }
}