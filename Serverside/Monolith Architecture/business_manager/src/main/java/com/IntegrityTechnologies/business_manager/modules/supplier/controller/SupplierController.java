package com.IntegrityTechnologies.business_manager.modules.supplier.controller;

import com.IntegrityTechnologies.business_manager.common.PageWrapper;
import com.IntegrityTechnologies.business_manager.modules.product.controller.ProductController;
import com.IntegrityTechnologies.business_manager.modules.supplier.dto.SupplierDTO;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.Supplier;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierAudit;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierImageAudit;
import com.IntegrityTechnologies.business_manager.modules.supplier.repository.SupplierAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.supplier.repository.SupplierImageAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.supplier.service.SupplierService;
import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.core.io.Resource;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.List;


@RestController
@RequestMapping("/api/suppliers")
@RequiredArgsConstructor
public class SupplierController {

    private final SupplierService supplierService;
    private final SupplierImageAuditRepository supplierImageAuditRepository;
    private SupplierAuditRepository supplierAuditRepository;

    @GetMapping("/all")
    public ResponseEntity<List<Supplier>> getAllSuppliers() {
        return ResponseEntity.ok(supplierService.getAllSuppliers());
    }

    @GetMapping("/{id}")
    public ResponseEntity<Supplier> getSupplier(@PathVariable Long id) {
        return ResponseEntity.ok(supplierService.getSupplier(id));
    }

    @GetMapping("/identifier/{identifier}")
    @Operation(summary = "Find supplier by name, email, or phone")
    public ResponseEntity<SupplierDTO> getByIdentifier(@PathVariable String identifier) {
        return ResponseEntity.ok(supplierService.getByIdentifier(identifier));
    }

    @GetMapping("/advanced")
    @Operation(summary = "Advanced supplier search and filtering")
    public ResponseEntity<PageWrapper<SupplierDTO>> advancedSearch(
            @RequestParam(required = false) List<Long> categoryIds,
            @RequestParam(required = false) String name,
            @RequestParam(required = false) String email,
            @RequestParam(required = false) String phone,
            @RequestParam(required = false) String region,
            @RequestParam(required = false) Double minRating,
            @RequestParam(required = false) LocalDateTime createdAfter,
            @RequestParam(required = false) LocalDateTime createdBefore,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String direction
    ) {
        var result = supplierService.advancedSearch(
                categoryIds, name, email, phone, region,
                minRating, createdAfter, createdBefore,
                page, size, sortBy, direction
        );
        return ResponseEntity.ok(new PageWrapper<>(result));
    }

    @GetMapping("/{id}/images")
    @Operation(summary = "Get all image URLs for a supplier")
    public ResponseEntity<List<String>> getSupplierImages(@PathVariable Long id) {
        return ResponseEntity.ok(supplierService.getSupplierImageUrls(id));
    }

    @GetMapping("/{id}/images/{filename}")
    @Operation(summary = "Download a specific supplier image")
    public ResponseEntity<Resource> downloadImage(@PathVariable Long id, @PathVariable String filename) {
        Resource file = supplierService.downloadImage(id, filename);
        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + filename + "\"")
                .body(file);
    }

    @GetMapping("/{id}/images/zip")
    @Operation(summary = "Download all supplier images as a ZIP")
    public ResponseEntity<Resource> downloadAllImagesZip(@PathVariable Long id) throws IOException {
        Resource zipFile = supplierService.downloadAllImagesAsZip(id);
        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"supplier_" + id + "_images.zip\"")
                .body(zipFile);
    }
    @GetMapping("/{id}/images/audit")
    @Operation(summary = "Get supplier image audit history")
    public ResponseEntity<List<SupplierImageAudit>> getSupplierImageAudit(@PathVariable Long id) {
        return ResponseEntity.ok(supplierImageAuditRepository.findBySupplierId(id));
    }

    @GetMapping("/{id}/audit")
    @Operation(summary = "Get supplier audit history")
    public ResponseEntity<List<SupplierAudit>> getSupplierAudit(@PathVariable Long id) {
        return ResponseEntity.ok(supplierAuditRepository.findBySupplierIdOrderByTimestampDesc(id));
    }


    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN', 'MANAGER')")
    @PostMapping
    @Operation(summary = "Create new supplier with optional images")
    public ResponseEntity<Supplier> createSupplier(
            @RequestPart Supplier supplier,
            @RequestParam(required = false) List<Long> categoryIds,
            @RequestParam(required = false) List<MultipartFile> images
    ) throws IOException {
        return ResponseEntity.ok(supplierService.createSupplier(supplier, categoryIds, images));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN', 'MANAGER')")
    @PutMapping("/{id}")
    @Operation(summary = "Update supplier information")
    public ResponseEntity<Supplier> updateSupplier(
            @PathVariable Long id,
            @RequestPart Supplier updated,
            @RequestParam(required = false) List<Long> categoryIds,
            @RequestParam(required = false) List<MultipartFile> newImages
    ) throws IOException {
        return ResponseEntity.ok(supplierService.updateSupplier(id, updated, categoryIds, newImages));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN', 'MANAGER')")
    @DeleteMapping("/soft/{id}")
    public ResponseEntity<Void> softDelete(@PathVariable Long id) {
        supplierService.softDeleteSupplier(id);
        return ResponseEntity.noContent().build();
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @PatchMapping("/restore/{id}")
    public ResponseEntity<Void> restore(@PathVariable Long id) {
        supplierService.restoreSupplier(id);
        return ResponseEntity.noContent().build();
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @DeleteMapping("/hard/{id}")
    public ResponseEntity<Void> hardDelete(@PathVariable Long id) {
        supplierService.hardDeleteSupplier(id);
        return ResponseEntity.noContent().build();
    }

    // ---------------------- IMAGE DELETION -------------------------

    @DeleteMapping("/{id}/images/{filename}")
    @Operation(summary = "Delete a specific supplier image")
    public ResponseEntity<Void> deleteSupplierImage(
            @PathVariable Long id,
            @PathVariable String filename) throws IOException {
        supplierService.deleteSupplierImage(id, filename);
        return ResponseEntity.noContent().build();
    }

    @DeleteMapping("/{id}/images")
    @Operation(summary = "Delete multiple supplier images")
    public ResponseEntity<Void> deleteSupplierImagesBulk(
            @PathVariable Long id,
            @RequestParam List<String> filenames) throws IOException {
        supplierService.deleteSupplierImagesBulk(id, filenames);
        return ResponseEntity.noContent().build();
    }
}