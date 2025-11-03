package com.IntegrityTechnologies.business_manager.modules.supplier.service;

import com.IntegrityTechnologies.business_manager.config.FileStorageProperties;
import com.IntegrityTechnologies.business_manager.modules.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.modules.supplier.dto.SupplierDTO;
import com.IntegrityTechnologies.business_manager.modules.supplier.mapper.SupplierMapper;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierAudit;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierImage;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierImageAudit;
import com.IntegrityTechnologies.business_manager.modules.supplier.repository.SupplierAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.supplier.repository.SupplierImageAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.supplier.repository.SupplierImageRepository;
import com.IntegrityTechnologies.business_manager.modules.supplier.repository.SupplierRepository;
import jakarta.persistence.EntityNotFoundException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.nio.file.*;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

@Service
@RequiredArgsConstructor
@Slf4j
public class SupplierService {

    private final SupplierRepository supplierRepository;
    private final SupplierImageRepository supplierImageRepository;
    private final CategoryRepository categoryRepository;
    private final SupplierMapper supplierMapper;
    private final FileStorageProperties fileStorageProperties;
    private final SupplierImageAuditRepository supplierImageAuditRepository;
    private final SupplierAuditRepository supplierAuditRepository;


    private Path suppliersRoot() {
        return Paths.get(fileStorageProperties.getSupplierUploadDir()).toAbsolutePath().normalize();
    }

    public List<Supplier> getAllSuppliers() {
        return supplierRepository.findByDeletedFalse();
    }

    public Supplier getSupplier(Long id) {
        return supplierRepository.findByIdAndDeletedFalse(id)
                .orElseThrow(() -> new EntityNotFoundException("Supplier not found"));
    }

    public SupplierDTO getByIdentifier(String identifier) {
        return supplierRepository.findByEmailIgnoreCase(identifier)
                .or(() -> supplierRepository.findByPhoneNumber(identifier))
                .or(() -> supplierRepository.findByNameIgnoreCase(identifier))
                .map(supplierMapper::toDTO)
                .orElseThrow(() -> new EntityNotFoundException("Supplier not found for identifier: " + identifier));
    }

    public Page<SupplierDTO> advancedSearch(List<Long> categoryIds, String name, String email, String phone,
                                            String region, Double minRating,
                                            LocalDateTime createdAfter, LocalDateTime createdBefore,
                                            int page, int size, String sortBy, String direction) {

        Specification<Supplier> spec = Specification.allOf(
                SupplierSpecification.inCategories(categoryIds),
                SupplierSpecification.hasNameLike(name),
                SupplierSpecification.hasEmail(email),
                SupplierSpecification.hasPhone(phone),
                SupplierSpecification.hasMinRating(minRating),
                SupplierSpecification.fromRegion(region),
                SupplierSpecification.createdBetween(createdAfter, createdBefore)
        );

        Pageable pageable = PageRequest.of(page, size,
                direction.equalsIgnoreCase("asc") ? Sort.by(sortBy).ascending() : Sort.by(sortBy).descending());

        return supplierRepository.findAll(spec, pageable).map(supplierMapper::toDTO);
    }

    public List<String> getSupplierImageUrls(Long supplierId) {
        return supplierImageRepository.findBySupplierId(supplierId)
                .stream().map(SupplierImage::getFilePath)
                .collect(Collectors.toList());
    }

    public Resource downloadImage(Long supplierId, String filename) {
        SupplierImage image = supplierImageRepository.findBySupplierId(supplierId)
                .stream().filter(img -> img.getFileName().equals(filename))
                .findFirst()
                .orElseThrow(() -> new EntityNotFoundException("Image not found"));

        return new FileSystemResource(image.getFilePath());
    }

    public Resource downloadAllImagesAsZip(Long supplierId) throws IOException {
        List<SupplierImage> images = supplierImageRepository.findBySupplierId(supplierId);
        if (images.isEmpty()) throw new EntityNotFoundException("No images found for supplier");

        Path tempZip = Files.createTempFile("supplier_" + supplierId + "_images_", ".zip");

        try (ZipOutputStream zos = new ZipOutputStream(Files.newOutputStream(tempZip))) {
            for (SupplierImage img : images) {
                Path imgPath = Paths.get(img.getFilePath());
                if (Files.exists(imgPath)) {
                    zos.putNextEntry(new ZipEntry(img.getFileName()));
                    Files.copy(imgPath, zos);
                    zos.closeEntry();
                }
            }
        }
        supplierImageAuditRepository.save(SupplierImageAudit.builder()
                .fileName("ALL")
                .action("ZIP_DOWNLOADED")
                .reason("All supplier images downloaded as ZIP")
                .timestamp(LocalDateTime.now())
                .supplier(supplierRepository.findById(supplierId).orElse(null))
                .performedBy(null)
                .build());

        return new FileSystemResource(tempZip);
    }


    public Supplier createSupplier(Supplier supplier, List<Long> categoryIds, List<MultipartFile> images) throws IOException {
        if (supplierRepository.existsByNameIgnoreCase(supplier.getName())) {
            throw new IllegalArgumentException("Supplier name already exists");
        }

        supplier.setCategories(new HashSet<>(categoryRepository.findAllById(categoryIds)));
        supplier.setCreatedAt(LocalDateTime.now());
        Supplier saved = supplierRepository.save(supplier);

        logSupplierAudit(saved, "CREATED", null, null, null, "New supplier added");

        if (images != null && !images.isEmpty()) uploadSupplierImages(saved, images);

        return supplierRepository.findById(saved.getId()).orElseThrow();
    }

    public void uploadSupplierImages(Supplier supplier, List<MultipartFile> images) throws IOException {
        Files.createDirectories(suppliersRoot());
        List<SupplierImage> supplierImages = new ArrayList<>();

        for (MultipartFile file : images) {
            String filename = UUID.randomUUID() + "_" + file.getOriginalFilename();
            Path filePath = suppliersRoot().resolve(filename);
            Files.write(filePath, file.getBytes());
            supplierImages.add(SupplierImage.builder()
                    .fileName(filename)
                    .filePath(filePath.toString())
                    .supplier(supplier)
                    .build());
        }

        supplierImageRepository.saveAll(supplierImages);
        supplier.setImages(supplierImages);
        supplierRepository.save(supplier);

// Audit log
        for (SupplierImage image : supplierImages) {
            SupplierImageAudit audit = SupplierImageAudit.builder()
                    .fileName(image.getFileName())
                    .filePath(image.getFilePath())
                    .action("UPLOADED")
                    .timestamp(LocalDateTime.now())
                    .supplier(supplier)
                    .performedBy(null) // or inject current user if available
                    .build();
            supplierImageAuditRepository.save(audit);
        }



    }

    public Supplier updateSupplier(Long id, Supplier updated, List<Long> categoryIds, List<MultipartFile> newImages) throws IOException {
        Supplier existing = getSupplier(id);

        // === Compare and audit field changes ===
        if (updated.getName() != null && !updated.getName().equals(existing.getName())) {
            logSupplierAudit(existing, "UPDATED", "name", existing.getName(), updated.getName(), "Supplier name updated");
            existing.setName(updated.getName());
        }

        if (updated.getEmail() != null && !updated.getEmail().equals(existing.getEmail())) {
            logSupplierAudit(existing, "UPDATED", "email", existing.getEmail(), updated.getEmail(), null);
            existing.setEmail(updated.getEmail());
        }

        if (updated.getPhoneNumber() != null && !updated.getPhoneNumber().equals(existing.getPhoneNumber())) {
            logSupplierAudit(existing, "UPDATED", "phoneNumber", existing.getPhoneNumber(), updated.getPhoneNumber(), null);
            existing.setPhoneNumber(updated.getPhoneNumber());
        }

        if (updated.getAddress() != null && !updated.getAddress().equals(existing.getAddress())) {
            logSupplierAudit(existing, "UPDATED", "address", existing.getAddress(), updated.getAddress(), null);
            existing.setAddress(updated.getAddress());
        }

        if (updated.getRegion() != null && !updated.getRegion().equals(existing.getRegion())) {
            logSupplierAudit(existing, "UPDATED", "region", existing.getRegion(), updated.getRegion(), null);
            existing.setRegion(updated.getRegion());
        }

        if (updated.getRating() != 0 && updated.getRating() != existing.getRating()) {
            logSupplierAudit(existing, "UPDATED", "rating",
                    String.valueOf(existing.getRating()),
                    String.valueOf(updated.getRating()),
                    null);
            existing.setRating(updated.getRating());
        }

        // === Update categories if provided ===
        if (categoryIds != null && !categoryIds.isEmpty()) {
            logSupplierAudit(existing, "UPDATED", "categories", null, categoryIds.toString(), "Updated supplier categories");
            existing.setCategories(new HashSet<>(categoryRepository.findAllById(categoryIds)));
        }

        // === Handle new images if any ===
        if (newImages != null && !newImages.isEmpty()) {
            uploadSupplierImages(existing, newImages);
            logSupplierAudit(existing, "UPDATED", "images", null, "Added new images", "Supplier image(s) added");
        }

        // === Update timestamps ===
        existing.setUpdatedAt(LocalDateTime.now());

        return supplierRepository.save(existing);
    }


    public void softDeleteSupplier(Long id) {
        Supplier supplier = getSupplier(id);
        supplier.setDeleted(true);
        supplier.setDeletedAt(LocalDateTime.now());
        supplierRepository.save(supplier);
        logSupplierAudit(supplier, "DELETED", null, null, null, "Soft deleted supplier");
    }

    public void restoreSupplier(Long id) {
        Supplier supplier = supplierRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Supplier not found"));
        supplier.setDeleted(false);
        supplier.setDeletedAt(null);
        supplierRepository.save(supplier);
        logSupplierAudit(supplier, "RESTORED", null, null, null, "Supplier restored");
    }

    public void hardDeleteSupplier(Long id) {
        supplierRepository.findById(id).ifPresent(supplier -> {
            logSupplierAudit(supplier, "HARD_DELETED", null, null, null, "Permanently deleted supplier");

            // Log image cascade
            supplierImageAuditRepository.save(
                    SupplierImageAudit.builder()
                            .fileName("ALL")
                            .action("CASCADE_DELETE")
                            .reason("Supplier deleted — all associated images removed")
                            .timestamp(LocalDateTime.now())
                            .supplier(supplier)
                            .build()
            );

            // Delete supplier images from disk
            Path supplierDir = suppliersRoot();
            try {
                Files.walk(supplierDir)
                        .filter(Files::isRegularFile)
                        .filter(p -> p.getFileName().toString().contains(supplier.getId().toString()))
                        .forEach(p -> {
                            try { Files.deleteIfExists(p); } catch (IOException ignored) {}
                        });
            } catch (IOException e) {
                log.warn("Failed to fully delete supplier images for {}", supplier.getId(), e);
            }

            supplierRepository.delete(supplier);
        });
    }

    // ---------------------- IMAGE DELETION -------------------------

    public void deleteSupplierImage(Long supplierId, String filename) throws IOException {
        SupplierImage image = supplierImageRepository.findBySupplierId(supplierId)
                .stream().filter(img -> img.getFileName().equals(filename))
                .findFirst()
                .orElseThrow(() -> new EntityNotFoundException("Image not found: " + filename));

        Path imgPath = Paths.get(image.getFilePath());
        if (Files.exists(imgPath)) Files.delete(imgPath);

        supplierImageRepository.delete(image);

        supplierImageAuditRepository.save(SupplierImageAudit.builder()
                .fileName(image.getFileName())
                .filePath(image.getFilePath())
                .action("DELETED")
                .reason("Manual deletion")
                .timestamp(LocalDateTime.now())
                .supplier(image.getSupplier())
                .performedBy(null)
                .build());

        log.info("Deleted image {} for supplier {}", filename, supplierId);
    }

    public void deleteSupplierImagesBulk(Long supplierId, List<String> filenames) throws IOException {
        if (filenames == null || filenames.isEmpty()) {
            throw new IllegalArgumentException("No filenames provided for deletion");
        }

        List<SupplierImage> images = supplierImageRepository.findBySupplierId(supplierId);
        List<SupplierImage> toDelete = images.stream()
                .filter(img -> filenames.contains(img.getFileName()))
                .collect(Collectors.toList());

        if (toDelete.isEmpty()) throw new EntityNotFoundException("No matching images found for deletion");

        for (SupplierImage image : toDelete) {
            Path imgPath = Paths.get(image.getFilePath());
            if (Files.exists(imgPath)) Files.delete(imgPath);

            supplierImageRepository.delete(image);

            supplierImageAuditRepository.save(SupplierImageAudit.builder()
                    .fileName(image.getFileName())
                    .filePath(image.getFilePath())
                    .action("DELETED")
                    .reason("Batch deletion")
                    .timestamp(LocalDateTime.now())
                    .supplier(image.getSupplier())
                    .performedBy(null)
                    .build());
        }

        log.info("Deleted {} images for supplier {}", toDelete.size(), supplierId);
    }

    private void logSupplierAudit(Supplier supplier, String action, String field, String oldVal, String newVal, String reason) {
        SupplierAudit audit = SupplierAudit.builder()
                .action(action)
                .fieldChanged(field)
                .oldValue(oldVal)
                .newValue(newVal)
                .reason(reason)
                .timestamp(LocalDateTime.now())
                .supplier(supplier)
                .performedBy(null) // Optional: inject current authenticated user
                .build();
        supplierAuditRepository.save(audit);
    }

}