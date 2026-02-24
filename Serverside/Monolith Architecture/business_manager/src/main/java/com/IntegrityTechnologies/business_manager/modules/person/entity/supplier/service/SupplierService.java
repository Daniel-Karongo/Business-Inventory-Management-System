package com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.service;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.common.FIleUploadDTO;
import com.IntegrityTechnologies.business_manager.config.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.TransactionalFileManager;
import com.IntegrityTechnologies.business_manager.exception.CategoryNotFoundException;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto.SupplierCreateDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto.SupplierDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto.SupplierUpdateDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.mapper.SupplierMapper;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.SupplierAudit;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.SupplierImage;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.SupplierImageAudit;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierImageAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierImageRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.CategorySupplier;
import com.IntegrityTechnologies.business_manager.modules.stock.category.model.CategorySupplierId;
import com.IntegrityTechnologies.business_manager.modules.stock.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.Hibernate;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.io.IOException;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

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


    /* ===================== read operations ===================== */

    public Supplier getSupplier(UUID id) {
        return supplierRepository.findByIdAndDeletedFalse(id)
                .orElseThrow(() -> new EntityNotFoundException("Supplier not found"));
    }

    public List<SupplierDTO> getSuppliers(Boolean deleted) {
        List<Supplier> suppliers = deleted == null
                ? supplierRepository.findAll()
                : deleted
                ? supplierRepository.findByDeletedTrue()
                : supplierRepository.findByDeletedFalse();

        return suppliers.stream()
                .map(supplierMapper::toDTO)
                .toList();
    }

    public SupplierDTO getByIdentifier(String identifier, Boolean deleted) {
        Optional<Supplier> maybe = Optional.empty();

        // 1️⃣ Try UUID
        try {
            UUID id = UUID.fromString(identifier);
            maybe = fetchSupplierById(id, deleted);
        } catch (IllegalArgumentException ignored) { }

        // 2️⃣ Try Email
        if (maybe.isEmpty()) {
            maybe = fetchSupplierByEmail(identifier, deleted);
        }

        // 3️⃣ Try Phone
        if (maybe.isEmpty()) {
            String phoneNormalized = normalizePhone(identifier);
            maybe = fetchSupplierByPhone(phoneNormalized, deleted);
        }

        // 4️⃣ Try Name
        if (maybe.isEmpty()) {
            maybe = fetchSupplierByName(identifier, deleted);
        }

        return maybe.map(supplierMapper::toDTO)
                .orElseThrow(() -> new EntityNotFoundException(
                        "Supplier not found for identifier: " + identifier
                ));
    }

    // Helper methods for clarity
    private Optional<Supplier> fetchSupplierById(UUID id, Boolean deleted) {
        if (deleted == null) return supplierRepository.findById(id);
        return deleted ? supplierRepository.findByIdAndDeletedTrue(id)
                : supplierRepository.findByIdAndDeletedFalse(id);
    }

    private Optional<Supplier> fetchSupplierByEmail(String email, Boolean deleted) {
        if (deleted == null) return supplierRepository.findByEmailElementIgnoreCase(email);
        return deleted ? supplierRepository.findByEmailElementIgnoreCaseAndDeletedTrue(email)
                : supplierRepository.findByEmailElementIgnoreCaseAndDeletedFalse(email);
    }

    private Optional<Supplier> fetchSupplierByPhone(String phone, Boolean deleted) {
        if (deleted == null) return supplierRepository.findByPhoneNumberElement(phone);
        return deleted ? supplierRepository.findByPhoneNumberElementAndDeletedTrue(phone)
                : supplierRepository.findByPhoneNumberElementAndDeletedFalse(phone);
    }

    private Optional<Supplier> fetchSupplierByName(String name, Boolean deleted) {
        if (deleted == null) return supplierRepository.findByNameIgnoreCase(name);
        return deleted ? supplierRepository.findByNameIgnoreCaseAndDeletedTrue(name)
                : supplierRepository.findByNameIgnoreCaseAndDeletedFalse(name);
    }

    public Page<SupplierDTO> advancedSearch(
            List<Long> categoryIds,
            String name,
            String email,
            String phone,
            String region,
            Double minRating,
            LocalDateTime createdAfter,
            LocalDateTime createdBefore,
            int page,
            int size,
            String sortBy,
            String direction,
            Boolean deleted) {

        var spec = org.springframework.data.jpa.domain.Specification.allOf(
                SupplierSpecification.inCategories(categoryIds),
                SupplierSpecification.hasNameLike(name),
                SupplierSpecification.hasEmail(email),
                SupplierSpecification.hasPhone(phone == null ? null : normalizePhone(phone)),
                SupplierSpecification.hasMinRating(minRating),
                SupplierSpecification.fromRegion(region),
                SupplierSpecification.createdBetween(createdAfter, createdBefore),
                SupplierSpecification.hasDeletedFlag(deleted)
        );

        var pageable = PageRequest.of(page, size,
                direction.equalsIgnoreCase("asc") ? Sort.by(sortBy).ascending() : Sort.by(sortBy).descending());

        return supplierRepository.findAll(spec, pageable).map(supplierMapper::toDTO);
    }

    public boolean existsByNameIgnoreCase(String name) {
        return supplierRepository.existsByNameIgnoreCase(name);
    }










    /**
     * Create supplier and optionally store uploaded images. File IO is delegated to SupplierFileStorageService.
     */
    @Transactional
    public SupplierDTO createSupplier(SupplierCreateDTO dto, String creatorUsername) throws IOException {
        // 1️⃣ Check if supplier name exists
        if (supplierRepository.existsByNameIgnoreCase(dto.getName())) {
            throw new IllegalArgumentException("Supplier name already exists");
        }

        // 2️⃣ Map DTO to entity
        Supplier supplier = supplierMapper.toEntity(dto);

        // 3️⃣ Normalize phone numbers
        if (supplier.getPhoneNumber() != null) {
            supplier.setPhoneNumber(
                    supplier.getPhoneNumber().stream()
                            .map(this::normalizePhone)
                            .collect(Collectors.toSet())
            );
        }

        // 4️⃣ Set creator
        if (creatorUsername != null) {
            userRepository.findByUsername(creatorUsername)
                    .ifPresent(supplier::setCreatedBy);
        }

        // 5️⃣ Generate unique upload folder
        supplier.setUploadFolder(UUID.randomUUID() + "_" + System.currentTimeMillis());

        // 6️⃣ Save supplier first to get ID
        supplier = supplierRepository.save(supplier);

        // 7️⃣ Handle categories
        if (dto.getCategoryIds() != null && !dto.getCategoryIds().isEmpty()) {
            List<Category> categories = categoryRepository.findAllById(dto.getCategoryIds());

            // Verify all categories exist
            if (categories.size() != dto.getCategoryIds().size()) {
                Set<Long> found = categories.stream().map(Category::getId).collect(Collectors.toSet());
                dto.getCategoryIds().stream()
                        .filter(catId -> !found.contains(catId))
                        .findFirst()
                        .ifPresent(catId -> {
                            throw new CategoryNotFoundException("Category not found: " + catId);
                        });
            }

            // Add supplier to categories
            for (Category cat : categories) {

                CategorySupplier relation = CategorySupplier.builder()
                        .id(new CategorySupplierId(cat.getId(), supplier.getId()))
                        .category(cat)
                        .supplier(supplier)
                        .build();

                cat.getCategorySuppliers().add(relation);
            }

            categoryRepository.saveAll(categories); // persist relationship
        }

        // 8️⃣ Handle images
        List<FIleUploadDTO> images = dto.getSupplierFiles();
        if (images != null && !images.isEmpty()) {
            Set<SupplierImage> imagesToSave = supplierFileStorageService.storeImages(supplier, images);
            supplierImageRepository.saveAll(imagesToSave);
            supplier.getImages().addAll(imagesToSave);

            supplierRepository.save(supplier); // persist relationship

            // Audit each image
            for (SupplierImage im : imagesToSave) {
                supplierImageAuditRepository.save(
                        SupplierImageAudit.builder()
                                .fileName(im.getFileName())
                                .filePath(im.getFilePath())
                                .action("UPLOADED")
                                .timestamp(LocalDateTime.now())
                                .supplierId(supplier.getId())
                                .supplierName(supplier.getName())
                                .performedBy(SecurityUtils.currentUsername())
                                .build()
                );
            }
        }

        // 9️⃣ Log supplier audit
        logSupplierAuditAsync(supplier, "CREATED_BULK", null, null, null, "New supplier added");

        // 10️⃣ Return DTO
        return supplierMapper.toDTO(
                supplierRepository.findById(supplier.getId())
                        .orElseThrow(() -> new IllegalStateException("Supplier not found after save"))
        );
    }

    @Transactional
    public Supplier createMinimalSupplier(
            String supplierName,
            Category category
    ) {

        Optional<Supplier> existing =
                supplierRepository.findByNameIgnoreCase(supplierName);

    /* =======================================================
       EXISTING SUPPLIER
    ======================================================= */

        if (existing.isPresent()) {

            Supplier supplier = existing.get();

            boolean alreadyLinked = category.getCategorySuppliers()
                    .stream()
                    .anyMatch(rel ->
                            rel.getSupplier().getId().equals(supplier.getId())
                    );

            if (!alreadyLinked) {

                CategorySupplier relation = CategorySupplier.builder()
                        .id(new CategorySupplierId(
                                category.getId(),
                                supplier.getId()
                        ))
                        .category(category)
                        .supplier(supplier)
                        .build();

                category.getCategorySuppliers().add(relation);
                categoryRepository.save(category);

                logSupplierAuditAsync(
                        supplier,
                        "UPDATED",
                        "categories",
                        null,
                        String.valueOf(category.getId()),
                        "Bulk import: linked supplier to category '" + category.getName() + "'"
                );
            }

            return supplier;
        }

    /* =======================================================
       CREATE MINIMAL SUPPLIER
    ======================================================= */

        Supplier supplier = Supplier.builder()
                .name(supplierName)
                .uploadFolder(UUID.randomUUID() + "_" + System.currentTimeMillis())
                .deleted(false)
                .build();

        supplier.setCreatedAt(LocalDateTime.now());

        supplier = supplierRepository.save(supplier);

    /* =======================================================
       AUDIT: CREATED
    ======================================================= */

        logSupplierAuditAsync(
                supplier,
                "CREATED",
                null,
                null,
                null,
                "Bulk import: minimal supplier auto-created"
        );

    /* =======================================================
       ATTACH TO CATEGORY (JOIN ENTITY)
    ======================================================= */

        CategorySupplier relation = CategorySupplier.builder()
                .id(new CategorySupplierId(
                        category.getId(),
                        supplier.getId()
                ))
                .category(category)
                .supplier(supplier)
                .build();

        category.getCategorySuppliers().add(relation);
        categoryRepository.save(category);

    /* =======================================================
       AUDIT: CATEGORY LINK
    ======================================================= */

        logSupplierAuditAsync(
                supplier,
                "UPDATED",
                "categories",
                null,
                String.valueOf(category.getId()),
                "Bulk import: supplier linked to category '" + category.getName() + "'"
        );

        return supplier;
    }

    @Transactional
    public SupplierDTO updateSupplier(UUID id, SupplierUpdateDTO dto, String updaterUsername) throws IOException {
        // 1️⃣ Fetch existing supplier
        Supplier existing = getSupplier(id);

        // 2️⃣ Apply basic updates via mapper
        supplierMapper.applyUpdate(existing, dto);

        // 3️⃣ Normalize phone numbers if provided
        if (dto.getPhoneNumber() != null) {
            existing.setPhoneNumber(
                    dto.getPhoneNumber().stream()
                            .map(this::normalizePhone)
                            .collect(Collectors.toSet())
            );
        }

        // 4️⃣ Update categories via Category entity only
        if (dto.getCategoryIds() != null) {

            // Clear old relations
            existing.getCategorySuppliers().clear();

            List<Category> newCategories = categoryRepository.findAllById(dto.getCategoryIds());

            if (newCategories.size() != dto.getCategoryIds().size()) {
                Set<Long> found = newCategories.stream().map(Category::getId).collect(Collectors.toSet());
                dto.getCategoryIds().stream()
                        .filter(catId -> !found.contains(catId))
                        .findFirst()
                        .ifPresent(catId -> {
                            throw new CategoryNotFoundException("Category not found: " + catId);
                        });
            }

            for (Category cat : newCategories) {

                CategorySupplier relation = CategorySupplier.builder()
                        .id(new CategorySupplierId(cat.getId(), existing.getId()))
                        .category(cat)
                        .supplier(existing)
                        .build();

                existing.getCategorySuppliers().add(relation);
            }

            logSupplierAuditAsync(
                    existing,
                    "UPDATED",
                    "categories",
                    null,
                    dto.getCategoryIds().toString(),
                    "Updated supplier categories"
            );
        }

        // 5️⃣ Update last modified info
        if (updaterUsername != null) {
            userRepository.findByUsername(updaterUsername)
                    .ifPresent(existing::setUpdatedBy);
        }
        existing.setUpdatedAt(LocalDateTime.now());

        // 6️⃣ Save and return updated DTO
        Supplier saved = supplierRepository.save(existing);
        return supplierMapper.toDTO(saved);
    }

    @Transactional
    public Map<String, Object> softDeleteSupplierInternal(UUID id) {
        Supplier supplier = getSupplier(id);
        supplier.setDeleted(true);
        supplier.setDeletedAt(LocalDateTime.now());
        supplierRepository.save(supplier);

        if (supplier.getImages() != null) {
            supplier.getImages().forEach(img -> {
                img.setDeleted(true);
                logSupplierImageAuditAsync("SOFT DELETED", supplier, SecurityUtils.currentUsername(), img, "Supplier soft deleted");
                supplierImageRepository.save(img);
            });
        }

        logSupplierAuditAsync(supplier, "DELETED", null, null, null, "Soft deleted supplier");

        // Only return the required fields
        return Map.of(
                "name", supplier.getName(),
                "email", supplier.getEmail(),
                "phoneNumber", supplier.getPhoneNumber()
        );
    }

    @Transactional
    public ResponseEntity<ApiResponse> softDeleteSupplier(UUID id) {
        Map<String, Object> deleted = softDeleteSupplierInternal(id);
        return ResponseEntity.ok(new ApiResponse("success", "Supplier soft deleted successfully", deleted));
    }

    @Transactional
    public ResponseEntity<ApiResponse> softDeleteSuppliersInBulk(List<UUID> supplierIds) {
        List<Map<String, Object>> deleted = supplierIds.stream()
                .map(this::softDeleteSupplierInternal)
                .toList();

        return ResponseEntity.ok(new ApiResponse("success", "Suppliers soft deleted successfully", deleted));
    }

    @Transactional
    public Map<String, Object> restoreSupplierInternal(UUID id) {
        Supplier supplier = supplierRepository.findById(id)
                .orElseThrow(() -> new EntityNotFoundException("Supplier not found"));

        supplier.setDeleted(false);
        supplier.setDeletedAt(null);
        supplierRepository.save(supplier);

        if (supplier.getImages() != null) {
            supplier.getImages().forEach(img -> {
                img.setDeleted(false);

                logSupplierImageAuditAsync("RESTORED", supplier, SecurityUtils.currentUsername(), img, "Soft deleted supplier restored");
                supplierImageRepository.save(img);
            });
        }

        logSupplierAuditAsync(supplier, "RESTORED", null, null, null, "Supplier restored");

        return Map.of(
                "name", supplier.getName(),
                "email", supplier.getEmail(),
                "phoneNumber", supplier.getPhoneNumber()
        );
    }

    @Transactional
    public ResponseEntity<ApiResponse> restoreSupplier(UUID id) {
        Map<String, Object> restored = restoreSupplierInternal(id);
        return ResponseEntity.ok(new ApiResponse("success", "Supplier restored successfully", restored));
    }

    @Transactional
    public ResponseEntity<ApiResponse> restoreSuppliersInBulk(List<UUID> supplierIds) {
        List<Map<String, Object>> restored = supplierIds.stream()
                .map(this::restoreSupplierInternal)
                .toList();

        return ResponseEntity.ok(new ApiResponse("success", "Suppliers restored successfully", restored));
    }



    /**
     * Deletes a supplier and its associated directory safely.
     * If file cleanup fails, the transaction is rolled back and no DB changes are committed.
     */
    @Transactional
    public ResponseEntity<ApiResponse> hardDeleteSupplier(UUID supplierId) {

        Supplier supplier = supplierRepository.findById(supplierId)
                .orElseThrow(() -> new EntityNotFoundException(
                        "Supplier with id %s not found".formatted(supplierId)
                ));

        return hardDeleteSuppliersInBulk(List.of(supplier.getId()));
    }

    @Transactional
    public ResponseEntity<ApiResponse> hardDeleteSuppliersInBulk(List<UUID> supplierIds) {

        if (supplierIds.isEmpty()) {
            return ResponseEntity.badRequest()
                    .body(new ApiResponse("error", "No supplier IDs provided", null));
        }

        // Load suppliers WITH associated images before deletion
        List<Supplier> suppliers = supplierRepository.findAllById(supplierIds);
        suppliers.forEach(s -> Hibernate.initialize(s.getImages()));

        // 👇 LOG BEFORE DATA IS DELETED — and in new transactions
        for (Supplier s : suppliers) {
            logSupplierAuditAsync(s, "HARD DELETED", null, null, null, "Supplier permanently deleted");

            for (SupplierImage img : s.getImages()) {
                logSupplierImageAuditAsync("HARD DELETED", s, SecurityUtils.currentUsername(), img, "Supplier hard deleted");
            }
        }

        // 👇 DELETE DB RECORDS
        supplierRepository.detachFromCategoriesBulk(supplierIds);
        supplierRepository.detachFromProductsBulk(supplierIds);
        supplierImageRepository.deleteAllBySupplierIds(supplierIds);
        supplierRepository.deleteSuppliersByIds(supplierIds);

        // 👇 DELETE FILES *after* transaction commits
        suppliers.forEach(s -> transactionalFileManager.runAfterCommit(() -> {
            try {
                Path dir = fileStorageService.supplierRoot()
                        .resolve(s.getUploadFolder())
                        .normalize();

                fileStorageService.deleteDirectory(dir);
            } catch (Exception e) {
                log.error("Failed to delete supplier directory {}: {}", s.getUploadFolder(), e.getMessage());
            }
        }));

        List<Map<String, String>> deleted = suppliers.stream()
                .map(s -> Map.of("name", s.getName()))
                .toList();

        return ResponseEntity.ok(new ApiResponse("success", "Suppliers permanently deleted", deleted));
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void logSupplierAuditAsync(
            Supplier supplier,
            String action,
            String field,
            String oldVal,
            String newVal,
            String reason
    ) {
        SupplierAudit audit = SupplierAudit.builder()
                .action(action)
                .fieldChanged(field)
                .oldValue(oldVal)
                .newValue(newVal)
                .reason(reason)
                .timestamp(LocalDateTime.now())
                .supplierId(supplier.getId())
                .supplierName(supplier.getName())
                .performedBy(SecurityUtils.currentUsername())
                .build();

        supplierAuditRepository.save(audit);
    }


    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void logSupplierImageAuditAsync(
            String action,
            Supplier supplier,
            String updaterUsername,
            SupplierImage image,
            String reason
    ) {
        SupplierImageAudit audit = SupplierImageAudit.builder()
                .fileName(image.getFileName())
                .filePath(image.getFilePath())
                .action(action)
                .reason(reason)
                .timestamp(LocalDateTime.now())
                .supplierId(supplier.getId())
                .supplierName(supplier.getName())
                .performedBy(updaterUsername)
                .build();

        supplierImageAuditRepository.save(audit);
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

    @Transactional
    public ResponseEntity<List<SupplierAudit>> getIndividualSupplierAudit(String identifier, Boolean deleted) {
        SupplierDTO supplier = getByIdentifier(identifier, deleted);
        return ResponseEntity.ok(supplierAuditRepository.findBySupplierIdOrderByTimestampDesc(supplier.getId()));
    }

    @Transactional
    public ResponseEntity<List<SupplierAudit>> getAllSuppliersAudits() {
        return ResponseEntity.ok(supplierAuditRepository.findAllByOrderByTimestampDesc());
    }
}