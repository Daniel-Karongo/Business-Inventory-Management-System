package com.IntegrityTechnologies.business_manager.modules.supplier.service;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.config.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.TransactionalFileManager;
import com.IntegrityTechnologies.business_manager.exception.CategoryNotFoundException;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.modules.supplier.dto.*;
import com.IntegrityTechnologies.business_manager.modules.supplier.mapper.SupplierMapper;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.*;
import com.IntegrityTechnologies.business_manager.modules.supplier.repository.*;
import com.IntegrityTechnologies.business_manager.modules.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.*;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.*;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * Supplier service now focuses on domain logic. All file IO is delegated to SupplierFileStorageService.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class SupplierService {

    private final SupplierRepository supplierRepository;
    private final SupplierImageRepository supplierImageRepository;
    private final CategoryRepository categoryRepository;
    private final SupplierMapper supplierMapper;
    private final FileStorageService fileStorageService;
    private final SupplierImageAuditRepository supplierImageAuditRepository;
    private final SupplierAuditRepository supplierAuditRepository;
    private final SupplierFileStorageService supplierFileStorageService;
    private final TransactionalFileManager transactionalFileManager;
    private final UserRepository userRepository;

    private String currentUser() {
        var auth = org.springframework.security.core.context.SecurityContextHolder.getContext().getAuthentication();
        return (auth != null && auth.getName() != null) ? auth.getName() : "SYSTEM";
    }

    public List<SupplierDTO> getAllSuppliers() {
        return supplierRepository.findAll().stream()
                .map(supplierMapper::toDTO).collect(Collectors.toList());
    }

    public List<SupplierDTO> getAllActiveSuppliers() {
        return supplierRepository.findByDeletedFalse().stream()
                .map(supplierMapper::toDTO).collect(Collectors.toList());
    }

    public List<SupplierDTO> getAllDeletedSuppliers() {
        return supplierRepository.findByDeletedTrue().stream()
                .map(supplierMapper::toDTO).collect(Collectors.toList());
    }

    public SupplierDTO getSupplierDto(UUID id) {
        Supplier supplier = supplierRepository.findByIdAndDeletedFalse(id)
                .orElseThrow(() -> new EntityNotFoundException("Supplier not found"));
        return supplierMapper.toDTO(supplier);
    }

    public Supplier getSupplier(UUID id) {
        return supplierRepository.findByIdAndDeletedFalse(id)
                .orElseThrow(() -> new EntityNotFoundException("Supplier not found"));
    }

    public SupplierDTO getByIdentifier(String identifier) {
        Optional<Supplier> maybe = supplierRepository.findByEmailElementIgnoreCase(identifier);
        if (maybe.isEmpty()) {
            String phoneNormalized = normalizePhone(identifier);
            maybe = supplierRepository.findByPhoneNumberElement(phoneNormalized);
        }
        if (maybe.isEmpty()) {
            maybe = supplierRepository.findByNameIgnoreCaseAndDeletedFalse(identifier);
        }
        return maybe.map(supplierMapper::toDTO)
                .orElseThrow(() -> new EntityNotFoundException("Supplier not found for identifier: " + identifier));
    }

    public Page<SupplierDTO> advancedSearch(List<Long> categoryIds, String name, String email, String phone,
                                            String region, Double minRating,
                                            LocalDateTime createdAfter, LocalDateTime createdBefore,
                                            int page, int size, String sortBy, String direction) {

        var spec = org.springframework.data.jpa.domain.Specification.allOf(
                SupplierSpecification.inCategories(categoryIds),
                SupplierSpecification.hasNameLike(name),
                SupplierSpecification.hasEmail(email),
                SupplierSpecification.hasPhone(phone == null ? null : normalizePhone(phone)),
                SupplierSpecification.hasMinRating(minRating),
                SupplierSpecification.fromRegion(region),
                SupplierSpecification.createdBetween(createdAfter, createdBefore)
        );

        var pageable = PageRequest.of(page, size,
                direction.equalsIgnoreCase("asc") ? Sort.by(sortBy).ascending() : Sort.by(sortBy).descending());

        return supplierRepository.findAll(spec, pageable).map(supplierMapper::toDTO);
    }

    public List<String> getSupplierImageUrls(UUID supplierId) {
        return supplierImageRepository.findBySupplierIdAndDeletedFalse(supplierId)
                .stream().map(img -> "/api/suppliers/" + supplierId + "/images/" + img.getFileName())
                .collect(Collectors.toList());
    }

    public Resource downloadImage(UUID supplierId, String filename) {
        SupplierImage image = Optional.ofNullable(supplierImageRepository.findBySupplierIdAndFileName(supplierId, filename))
                .orElseThrow(() -> new EntityNotFoundException("Image not found"));
        Path path = Paths.get(image.getFilePath());
        if (!Files.exists(path)) throw new EntityNotFoundException("Image file not found on disk");
        return new FileSystemResource(path.toFile());
    }

    /**
     * Stream zip of supplier images using the storage service's paths.
     */
    public void streamAllImagesAsZip(UUID supplierId, OutputStream out) throws IOException {
        List<SupplierImage> images = supplierImageRepository.findBySupplierIdAndDeletedFalse(supplierId);
        if (images.isEmpty()) throw new EntityNotFoundException("No images found for supplier");

        try (ZipOutputStream zos = new ZipOutputStream(out)) {
            for (SupplierImage img : images) {
                Path imgPath = Paths.get(img.getFilePath());
                if (!Files.exists(imgPath)) continue;
                zos.putNextEntry(new ZipEntry(img.getFileName()));
                try (InputStream in = Files.newInputStream(imgPath)) {
                    in.transferTo(zos);
                }
                zos.closeEntry();
            }
            zos.finish();
        }

        supplierImageAuditRepository.save(SupplierImageAudit.builder()
                .fileName("ALL")
                .action("ZIP_DOWNLOADED")
                .reason("All supplier images downloaded as ZIP")
                .timestamp(LocalDateTime.now())
                .supplierId(supplierId)
                .supplierName(supplierRepository.findById(supplierId).get().getName())
                .performedBy(currentUser())
                .build());
    }

    /**
     * Create supplier and optionally store uploaded images. File IO is delegated to SupplierFileStorageService.
     */
    @Transactional
    public SupplierDTO createSupplier(SupplierCreateDTO dto, String creatorUsername) throws IOException {
        if (supplierRepository.existsByNameIgnoreCase(dto.getName())) {
            throw new IllegalArgumentException("Supplier name already exists");
        }

        Supplier supplier = supplierMapper.toEntity(dto);

        if (supplier.getPhoneNumber() != null) {
            supplier.setPhoneNumber(supplier.getPhoneNumber().stream().map(this::normalizePhone).collect(Collectors.toList()));
        }

        if (dto.getCategoryIds() != null && !dto.getCategoryIds().isEmpty()) {
            var cats = categoryRepository.findAllById(dto.getCategoryIds());
            if (cats.size() != dto.getCategoryIds().size()) {
                Set<Long> found = cats.stream().map(c -> c.getId()).collect(Collectors.toSet());
                dto.getCategoryIds().stream().filter(id -> !found.contains(id)).findFirst()
                        .ifPresent(id -> { throw new CategoryNotFoundException("Category not found: " + id); });
            }
            supplier.setCategories(new HashSet<>(cats));
        }

        if (creatorUsername != null) {
            userRepository.findByUsername(creatorUsername)
                    .ifPresent(supplier::setCreatedBy);
        }

        supplier.setCreatedAt(LocalDateTime.now());
        supplier = supplierRepository.save(supplier);

        // Store images (delegated). TransactionalFileManager will delete files on rollback.
        List<MultipartFile> images = dto.getImages();
        if (images != null && !images.isEmpty()) {
            List<SupplierImage> imagesToSave = supplierFileStorageService.storeImages(supplier, images);
            supplierImageRepository.saveAll(imagesToSave);
            supplier.getImages().addAll(imagesToSave);
            supplierRepository.save(supplier); // persist relationship

            // audits
            for (SupplierImage im : imagesToSave) {
                supplierImageAuditRepository.save(SupplierImageAudit.builder()
                        .fileName(im.getFileName())
                        .filePath(im.getFilePath())
                        .action("UPLOADED")
                        .timestamp(LocalDateTime.now())
                        .supplierId(supplier.getId())
                        .supplierName(supplier.getName())
                        .performedBy(currentUser())
                        .build());
            }
        }

        logSupplierAudit(supplier, "CREATED", null, null, null, "New supplier added");
        return supplierMapper.toDTO(supplierRepository.findById(supplier.getId()).orElseThrow());
    }

    @Transactional
    public SupplierDTO updateSupplier(UUID id, SupplierUpdateDTO dto, String updaterUsername) throws IOException {
        Supplier existing = getSupplier(id);

        supplierMapper.applyUpdate(existing, dto);

        if (dto.getPhoneNumber() != null) {
            existing.setPhoneNumber(dto.getPhoneNumber().stream().map(this::normalizePhone).collect(Collectors.toList()));
        }

        if (dto.getCategoryIds() != null && !dto.getCategoryIds().isEmpty()) {
            var cats = categoryRepository.findAllById(dto.getCategoryIds());
            if (cats.size() != dto.getCategoryIds().size()) {
                Set<Long> found = cats.stream().map(c -> c.getId()).collect(Collectors.toSet());
                dto.getCategoryIds().stream().filter(cid -> !found.contains(cid)).findFirst()
                        .ifPresent(cid -> { throw new CategoryNotFoundException("Category not found: " + cid); });
            }
            existing.setCategories(new HashSet<>(cats));
            logSupplierAudit(existing, "UPDATED", "categories", null, dto.getCategoryIds().toString(), "Updated supplier categories");
        }

        if (updaterUsername != null) {
            userRepository.findByUsername(updaterUsername)
                    .ifPresent(existing::setUpdatedBy);
        }
        existing.setUpdatedAt(LocalDateTime.now());
        Supplier saved = supplierRepository.save(existing);
        return supplierMapper.toDTO(saved);
    }

    @Transactional
    public SupplierDTO updateSupplierImages(UUID id, List<MultipartFile> newImages, String updaterUsername) throws IOException {
        Supplier existing = getSupplier(id);

        if (newImages != null && !newImages.isEmpty()) {
            List<SupplierImage> saved = supplierFileStorageService.storeImages(existing, newImages);
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
            logSupplierAudit(existing, "UPDATED", "images", null, "Added new images", "Supplier image(s) added");
        }

        if (updaterUsername != null) {
            userRepository.findByUsername(updaterUsername)
                    .ifPresent(existing::setUpdatedBy);
        }
        existing.setUpdatedAt(LocalDateTime.now());
        Supplier saved = supplierRepository.save(existing);
        return supplierMapper.toDTO(saved);
    }

    @Transactional
    public ResponseEntity<ApiResponse> softDeleteSupplier(UUID id) {
        Supplier supplier = getSupplier(id);
        supplier.setDeleted(true);
        supplier.setDeletedAt(LocalDateTime.now());
        supplierRepository.save(supplier);
        logSupplierAudit(supplier, "DELETED", null, null, null, "Soft deleted supplier");

        // Mark images deleted
        if (supplier.getImages() != null) {
            for (SupplierImage img : supplier.getImages()) {
                img.setDeleted(true);
                supplierImageRepository.save(img);
            }
        }
        ApiResponse response = new ApiResponse("success", "Supplier soft deleted successfully");
        return ResponseEntity.ok(response);
    }

    @Transactional
    public ResponseEntity<ApiResponse> restoreSupplier(UUID id) {
        Supplier supplier = supplierRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Supplier not found"));
        supplier.setDeleted(false);
        supplier.setDeletedAt(null);
        supplierRepository.save(supplier);
        logSupplierAudit(supplier, "RESTORED", null, null, null, "Supplier restored");

        // Mark images as not-deleted
        if (supplier.getImages() != null) {
            for (SupplierImage img : supplier.getImages()) {
                img.setDeleted(false);
                supplierImageRepository.save(img);
            }
        }

        ApiResponse response = new ApiResponse("success", "Supplier restored successfully");
        return ResponseEntity.ok(response);
    }

    /**
     * Deletes a supplier and its associated directory safely.
     * If file cleanup fails, the transaction is rolled back and no DB changes are committed.
     */
    @Transactional
    public ResponseEntity<ApiResponse> hardDeleteSupplier(UUID supplierId) {
        Supplier supplier = supplierRepository.findById(supplierId)
                .orElseThrow(() -> new EntityNotFoundException("Supplier not found: " + supplierId));

        // Remove dependent entities first to avoid FK constraint issues
        supplierImageRepository.deleteAllBySupplierId(supplierId);
        supplierRepository.delete(supplier);

        log.debug("Supplier {} DB records deleted successfully. Scheduling file cleanup...", supplierId);

//         ✅ Post-commit file cleanup (after DB commit succeeds)
        transactionalFileManager.runAfterCommit(() -> {
            try {
                log.debug("Running post-commit cleanup for supplier {}", supplierId);
                Path supplierDir = fileStorageService.resolveModulePath("suppliers/supplier-" + supplierId);
                System.out.println("Hello");
                System.out.println(supplierDir);
                fileStorageService.deleteVisibleOrHiddenDirectory(supplierDir);
                log.info("Supplier {} directories cleaned successfully.", supplierId);
            } catch (Exception e) {
                log.error("Failed to delete supplier {} directories: {}", supplierId, e.getMessage(), e);
            }
        });

        logSupplierAudit(supplier, "HARD DELETED", null, null, null, "Supplier deleted permanently");
        ApiResponse response = new ApiResponse("success", "Supplier deleted permanently");
        return ResponseEntity.ok(response);
    }

    @Transactional
    public ResponseEntity<ApiResponse> deleteSupplierImage(UUID supplierId, String filename) throws IOException {
        SupplierImage image = Optional.ofNullable(supplierImageRepository.findBySupplierIdAndFileName(supplierId, filename))
                .orElseThrow(() -> new EntityNotFoundException("Image not found: " + filename));

        supplierImageRepository.delete(image);

        Path imgPath = Paths.get(image.getFilePath());
        try {
            supplierFileStorageService.deleteFile(imgPath);
        } catch (IOException e) {
            log.warn("Failed to delete file {}: {}", imgPath, e.getMessage());
        }

        supplierImageAuditRepository.save(SupplierImageAudit.builder()
                .fileName(image.getFileName())
                .filePath(image.getFilePath())
                .action("DELETED")
                .reason("Manual deletion")
                .timestamp(LocalDateTime.now())
                .supplierId(supplierId)
                .supplierName(supplierRepository.findById(supplierId).get().getName())
                .performedBy(currentUser())
                .build());

        log.info("Deleted image {} for supplier {}", filename, supplierId);
        ApiResponse response = new ApiResponse("success", "Image deleted successfully");
        return ResponseEntity.ok(response);
    }

    @Transactional
    public ResponseEntity<ApiResponse> deleteSupplierImagesBulk(UUID supplierId, List<String> filenames) throws IOException {
        if (filenames == null || filenames.isEmpty()) throw new IllegalArgumentException("No filenames provided for deletion");
        List<SupplierImage> images = supplierImageRepository.findBySupplierIdAndDeletedFalse(supplierId);
        List<SupplierImage> toDelete = images.stream().filter(i -> filenames.contains(i.getFileName())).collect(Collectors.toList());
        if (toDelete.isEmpty()) throw new EntityNotFoundException("No matching images found for deletion");

        // Batch delete
        supplierImageRepository.deleteAll(toDelete);

        for (SupplierImage image : toDelete) {
            Path imgPath = Paths.get(image.getFilePath());
            try { supplierFileStorageService.deleteFile(imgPath); } catch (IOException e) { log.warn("File deletion failed: {}", imgPath, e); }

            supplierImageAuditRepository.save(SupplierImageAudit.builder()
                    .fileName(image.getFileName())
                    .filePath(image.getFilePath())
                    .action("DELETED")
                    .reason("Batch deletion")
                    .timestamp(LocalDateTime.now())
                    .supplierId(supplierId)
                    .supplierName(supplierRepository.findById(supplierId).get().getName())
                    .performedBy(currentUser())
                    .build());
        }
        log.info("Deleted {} images for supplier {}", toDelete.size(), supplierId);
        ApiResponse response = new ApiResponse("success", "Images deleted successfully");
        return ResponseEntity.ok(response);
    }

    private void logSupplierAudit(Supplier supplier, String action, String field, String oldVal, String newVal, String reason) {
        SupplierAudit audit = SupplierAudit.builder()
                .action(action)
                .fieldChanged(field)
                .oldValue(oldVal)
                .newValue(newVal)
                .reason(reason)
                .timestamp(LocalDateTime.now())
                .supplierId(supplier.getId())
                .supplierName(supplier.getName())
                .performedBy(currentUser())
                .build();
        supplierAuditRepository.save(audit);
    }

    private String normalizePhone(String phone) {
        if (phone == null) return null;

        // 1️⃣ Remove spaces and hyphens
        String cleaned = phone.replaceAll("[\\s-]", "");

        // 2️⃣ Convert local formats to international
        if (cleaned.matches("^07\\d{7,8}$")) {
            cleaned = "+254" + cleaned.substring(1);
        } else if (cleaned.matches("^01\\d{7,8}$")) {
            cleaned = "+254" + cleaned.substring(1);
        }

        return cleaned;
    }
}