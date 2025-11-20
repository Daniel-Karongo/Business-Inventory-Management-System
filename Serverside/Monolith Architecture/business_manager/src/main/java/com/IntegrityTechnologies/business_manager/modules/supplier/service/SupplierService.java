package com.IntegrityTechnologies.business_manager.modules.supplier.service;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.config.FileStorageService;
import com.IntegrityTechnologies.business_manager.config.TransactionalFileManager;
import com.IntegrityTechnologies.business_manager.exception.CategoryNotFoundException;
import com.IntegrityTechnologies.business_manager.exception.EntityNotFoundException;
import com.IntegrityTechnologies.business_manager.exception.ImageNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.category.model.Category;
import com.IntegrityTechnologies.business_manager.modules.category.repository.CategoryRepository;
import com.IntegrityTechnologies.business_manager.modules.product.model.Product;
import com.IntegrityTechnologies.business_manager.modules.product.repository.ProductRepository;
import com.IntegrityTechnologies.business_manager.modules.supplier.dto.*;
import com.IntegrityTechnologies.business_manager.modules.supplier.mapper.SupplierMapper;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.*;
import com.IntegrityTechnologies.business_manager.modules.supplier.repository.*;
import com.IntegrityTechnologies.business_manager.modules.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.*;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.*;
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
    private final FileStorageService fileStorageService;
    private final SupplierImageAuditRepository supplierImageAuditRepository;
    private final SupplierAuditRepository supplierAuditRepository;
    private final SupplierFileStorageService supplierFileStorageService;
    private final TransactionalFileManager transactionalFileManager;
    private final UserRepository userRepository;
    private final ProductRepository productRepository;

    public String currentUser() {
        var auth = org.springframework.security.core.context.SecurityContextHolder.getContext().getAuthentication();
        return (auth != null && auth.getName() != null) ? auth.getName() : "SYSTEM";
    }

    /* ===================== read operations ===================== */

    public List<SupplierDTO> getAllSuppliers() {
        // This now fetches categories, images, emails, phone numbers eagerly
        return supplierMapper.toDTOList(supplierRepository.findAll());
    }

    public List<SupplierDTO> getAllActiveSuppliers() {
        return supplierRepository.findByDeletedFalse().stream().map(supplierMapper::toDTO).collect(Collectors.toList());
    }

    public List<SupplierDTO> getAllDeletedSuppliers() {
        return supplierRepository.findByDeletedTrue().stream().map(supplierMapper::toDTO).collect(Collectors.toList());
    }

    public Supplier getSupplier(UUID id) {
        return supplierRepository.findByIdAndDeletedFalse(id)
                .orElseThrow(() -> new EntityNotFoundException("Supplier not found"));
    }

    public SupplierDTO getByIdentifier(String identifier, Boolean deleted) {

        Optional<Supplier> maybe = Optional.empty();

        UUID id = UUID.fromString(identifier);
        // 🔍 1️⃣ Try id lookup
        if (deleted == null)
            maybe = supplierRepository.findById(id);
        else if (deleted)
            maybe = supplierRepository.findByIdAndDeletedTrue(id);
        else
            maybe = supplierRepository.findByIdAndDeletedFalse(id);


        // 🔍 1️⃣ Try email lookup
        if (deleted == null)
            maybe = supplierRepository.findByEmailElementIgnoreCase(identifier);
        else if (deleted)
            maybe = supplierRepository.findByEmailElementIgnoreCaseAndDeletedTrue(identifier);
        else
            maybe = supplierRepository.findByEmailElementIgnoreCaseAndDeletedFalse(identifier);


        // 🔍 2️⃣ Try phone lookup
        if (maybe.isEmpty()) {
            String phoneNormalized = normalizePhone(identifier);

            if (deleted == null)
                maybe = supplierRepository.findByPhoneNumberElement(phoneNormalized);
            else if (deleted)
                maybe = supplierRepository.findByPhoneNumberElementAndDeletedTrue(phoneNormalized);
            else
                maybe = supplierRepository.findByPhoneNumberElementAndDeletedFalse(phoneNormalized);
        }


        // 🔍 3️⃣ Try name lookup
        if (maybe.isEmpty()) {
            if (deleted == null)
                maybe = supplierRepository.findByNameIgnoreCase(identifier);
            else if (deleted)
                maybe = supplierRepository.findByNameIgnoreCaseAndDeletedTrue(identifier);
            else
                maybe = supplierRepository.findByNameIgnoreCaseAndDeletedFalse(identifier);
        }

        return maybe.map(supplierMapper::toDTO)
                .orElseThrow(() -> new EntityNotFoundException(
                        "Supplier not found for identifier: " + identifier
                ));
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
            supplier.setPhoneNumber(supplier.getPhoneNumber().stream().map(this::normalizePhone).collect(Collectors.toSet()));
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
        String uploadFolder = UUID.randomUUID() + "_" + System.currentTimeMillis();
        supplier.setUploadFolder(uploadFolder);
        supplier = supplierRepository.save(supplier);

        // Store images (delegated). TransactionalFileManager will delete files on rollback.
        Set<MultipartFile> images = dto.getImages();
        if (images != null && !images.isEmpty()) {
            Set<SupplierImage> imagesToSave = supplierFileStorageService.storeImages(supplier, images);
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
            existing.setPhoneNumber(
                    dto.getPhoneNumber().stream()
                            .map(this::normalizePhone)
                            .collect(Collectors.toSet())
            );
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
    public Map<String, Object> softDeleteSupplierInternal(UUID id) {
        Supplier supplier = getSupplier(id);
        supplier.setDeleted(true);
        supplier.setDeletedAt(LocalDateTime.now());
        supplierRepository.save(supplier);

        if (supplier.getImages() != null) {
            supplier.getImages().forEach(img -> {
                img.setDeleted(true);
                supplierImageRepository.save(img);
            });
        }

        logSupplierAudit(supplier, "DELETED", null, null, null, "Soft deleted supplier");

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
                supplierImageRepository.save(img);
            });
        }

        logSupplierAudit(supplier, "RESTORED", null, null, null, "Supplier restored");

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
        return hardDeleteSuppliersInBulk(List.of(supplierId));
    }

    @Transactional
    public ResponseEntity<ApiResponse> hardDeleteSuppliersInBulk(List<UUID> supplierIds) {
        if (supplierIds.isEmpty()) {
            return ResponseEntity.badRequest()
                    .body(new ApiResponse("error", "No supplier IDs provided", null));
        }

        // Fetch suppliers before deletion
        List<Supplier> suppliers = supplierRepository.findAllById(supplierIds);

        // Prepare response data
        List<Map<String, String>> deleted = suppliers.stream()
                .map(s -> Map.of(
                        "name", s.getName()
                ))
                .toList();

        // 1️⃣ Bulk detach from categories
        supplierRepository.detachFromCategoriesBulk(supplierIds);

        // 2️⃣ Bulk detach from products
        supplierRepository.detachFromProductsBulk(supplierIds);

        // 3️⃣ Bulk delete related images
        supplierImageRepository.deleteAllBySupplierIds(supplierIds);

        // 4️⃣ Bulk delete suppliers
        supplierRepository.deleteSuppliersByIds(supplierIds);

        // 5️⃣ Schedule folder deletion and audit logs after commit
        for (Supplier s : suppliers) {
            String uploadFolder = s.getUploadFolder();
            transactionalFileManager.runAfterCommit(() -> {
                try {
                    Path supplierDir = supplierFileStorageService.getSupplierDirectory(uploadFolder);
                    fileStorageService.deleteVisibleOrHiddenDirectory(supplierDir);
                } catch (Exception e) {
                    log.error("Failed to delete supplier directory {}: {}", uploadFolder, e.getMessage());
                }
            });

            logSupplierAudit(s, "HARD DELETED", null, null, null, "Supplier permanently deleted");
        }

        return ResponseEntity.ok(new ApiResponse("success", "Suppliers permanently deleted", deleted));
    }

    public void logSupplierAudit(Supplier supplier, String action, String field, String oldVal, String newVal, String reason) {
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