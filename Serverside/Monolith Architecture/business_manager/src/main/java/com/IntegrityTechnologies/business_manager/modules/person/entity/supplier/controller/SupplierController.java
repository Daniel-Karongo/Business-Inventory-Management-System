package com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.controller;

import com.IntegrityTechnologies.business_manager.common.FIleUploadDTO;
import com.IntegrityTechnologies.business_manager.common.PageWrapper;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierImageAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.service.SupplierImageService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.service.SupplierService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.http.*;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Tag(name = "Suppliers")
@RestController
@RequestMapping("/api/suppliers")
@RequiredArgsConstructor
public class SupplierController {

    private final SupplierService supplierService;
    private final SupplierImageAuditRepository supplierImageAuditRepository;
    private final SupplierAuditRepository supplierAuditRepository;
    private final SupplierImageService supplierImageService;





    /* ====================== CREATE / UPDATE ====================== */

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PostMapping(value="/register", consumes = MediaType.MULTIPART_FORM_DATA_VALUE,produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<SupplierDTO> registerSupplier(
            @ModelAttribute("supplierDTO") @Valid SupplierCreateDTO supplierDTO,
            Authentication authentication
    ) throws IOException {
        String creatorUsername = (authentication != null && authentication.getPrincipal() instanceof UserDetails userDetails)
                ? userDetails.getUsername()
                : null;

        SupplierDTO created = supplierService.createSupplier(supplierDTO, creatorUsername);
        return ResponseEntity.status(HttpStatus.CREATED).body(created);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PostMapping(
            value = "/register/bulk",
            consumes = MediaType.MULTIPART_FORM_DATA_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE
    )
    public ResponseEntity<List<SupplierDTO>> registerSuppliersInBulk(
            @ModelAttribute SupplierBulkWithFilesDTO bulkDTO,
            Authentication authentication
    ) throws IOException {
        String creatorUsername = (authentication != null && authentication.getPrincipal() instanceof UserDetails ud)
                ? ud.getUsername()
                : null;
        List<SupplierDTO> savedSupplierDTOs = new ArrayList<>();
        for (SupplierCreateDTO supplierCreateDTO : bulkDTO.getSuppliers()) {
            savedSupplierDTOs.add(supplierService.createSupplier(supplierCreateDTO, creatorUsername));
        }
        return ResponseEntity.status(HttpStatus.CREATED).body(savedSupplierDTOs);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PatchMapping("/{id}")
    public ResponseEntity<SupplierDTO> updateSupplier(
            @PathVariable UUID id,
            @RequestBody @Valid SupplierUpdateDTO updated,
            Authentication authentication
    ) throws IOException {
        String updaterUsername = (authentication != null && authentication.getPrincipal() instanceof UserDetails userDetails)
                ? userDetails.getUsername()
                : null;
        SupplierDTO updatedSupplier = supplierService.updateSupplier(id, updated, updaterUsername);
        return ResponseEntity.ok(updatedSupplier);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PatchMapping(value="/{id}/images")
    public ResponseEntity<SupplierDTO> updateSupplierImages (
            @PathVariable UUID id,
            @RequestParam("newImages") @Valid List<FIleUploadDTO> newImages,
            Authentication authentication
    ) throws IOException {
        String updaterUsername = (authentication != null && authentication.getPrincipal() instanceof UserDetails userDetails)
                ? userDetails.getUsername()
                : null;
        SupplierDTO updatedSupplier = supplierImageService.updateSupplierImages(id, newImages, updaterUsername);
        return ResponseEntity.ok(updatedSupplier);
    }







    /* ====================== READ OPERATIONS ====================== */

    // ✅ Unified endpoint for fetching all suppliers
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER', 'SUPERVISOR')")
    @GetMapping(value = "/all", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get suppliers (active, deleted, or any)")
    public ResponseEntity<List<SupplierDTO>> getSuppliers(
            @RequestParam(required = false) Boolean deleted // null = all, true = deleted, false = active
    ) {
        return ResponseEntity.ok(supplierService.getSuppliers(deleted));
    }

    // ✅ Unified endpoint for fetching a supplier by identifier
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER', 'SUPERVISOR')")
    @GetMapping(value = "/identifier/{identifier}", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Find supplier by UUID, name, email, or phone (active, deleted, or any)")
    public ResponseEntity<SupplierDTO> getSupplierByIdentifier(
            @PathVariable String identifier,
            @RequestParam(required = false) Boolean deleted
    ) {
        return ResponseEntity.ok(supplierService.getByIdentifier(identifier, deleted));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER', 'SUPERVISOR')")
    @GetMapping(value = "/advanced", produces = MediaType.APPLICATION_JSON_VALUE)
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
            @RequestParam(defaultValue = "asc") String direction,
            @RequestParam(required = false) Boolean deleted
    ) {
        var result = supplierService.advancedSearch(
                categoryIds, name, email, phone, region,
                minRating, createdAfter, createdBefore,
                page, size, sortBy, direction, deleted
        );
        return ResponseEntity.ok(new PageWrapper<>(result));
    }








    /* ====================== IMAGE MANAGEMENT ====================== */

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @GetMapping("/{id}/images")
    @Operation(summary = "List supplier images (active, deleted, or all)")
    public ResponseEntity<List<SupplierImageDTO>> listSupplierImages(
            @PathVariable UUID id,
            @RequestParam(required = false) Boolean deleted
    ) {
        return ResponseEntity.ok(
                supplierImageService.listImages(id, deleted)
        );
    }

    /* ====================== IMAGE URLS ====================== */
//    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER', 'SUPERVISOR')")
//    @GetMapping(value = "/{id}/images", produces = MediaType.APPLICATION_JSON_VALUE)
//    @Operation(summary = "Get all URLs for supplier images (active, deleted, or any)")
//    public ResponseEntity<List<String>> getSupplierImages(
//            @PathVariable UUID id,
//            @RequestParam(required = false) Boolean deleted // null = any, true = deleted, false = active
//    ) {
//        return ResponseEntity.ok(supplierImageService.getSupplierImageUrls(id, deleted));
//    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @GetMapping("/images/all")
    public ResponseEntity<List<String>> getAllImages(
            @RequestParam(required = false) Boolean deletedSupplier,
            @RequestParam(required = false) Boolean deletedImage
    ) {
        return ResponseEntity.ok(supplierImageService.getAllSuppliersImages(deletedSupplier, deletedImage));
    }


    /* ====================== IMAGES DOWNLOADS ====================== */

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @GetMapping("/{id}/images/{filename}")
    @Operation(summary = "Download supplier image (active, deleted, or any)")
    public ResponseEntity<Resource> downloadSupplierImage(
            @PathVariable UUID id,
            @PathVariable String filename,
            @RequestParam(required = false) Boolean deleted // null = any, true = deleted, false = active
    ) throws IOException {

        Resource file = supplierImageService.downloadImage(id, filename, deleted);

        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION,
                        "attachment; filename=\"" + filename + "\"")
                .body(file);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @GetMapping("/{id}/images/zip")
    @Operation(summary = "Download supplier images as ZIP")
    public void downloadSupplierImagesZip(
            @PathVariable UUID id,
            @RequestParam(required = false) Boolean deleted, // null = any, true = deleted, false = active
            HttpServletResponse response
    ) throws IOException {

        List<SupplierImage> images = supplierImageService.getImages(id, deleted);
        Supplier supplier = supplierImageService.getSupplier(id);

        response.setContentType("application/zip");
        String state = deleted == null ? "all" : deleted ? "deleted" : "active";
        response.setHeader(HttpHeaders.CONTENT_DISPOSITION,
                "attachment; filename=\"supplier_" + id + "_" + state + "_images.zip\"");

        supplierImageService.writeImagesToZip(images, supplier, response.getOutputStream());
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @GetMapping("/images/all/download")
    public ResponseEntity<Resource> downloadAllImages(
            @RequestParam(required = false) Boolean deletedSupplier,
            @RequestParam(required = false) Boolean deletedImage
    ) throws IOException {
        return supplierImageService.downloadAllSuppliersImages(deletedSupplier, deletedImage);
    }





    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @DeleteMapping("/{id}/images/{filename}/soft")
    public ResponseEntity<?> softDeleteSupplierImage(@PathVariable UUID id, @PathVariable String filename)
            throws IOException {
        return supplierImageService.softDeleteSupplierImage(id, filename);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @DeleteMapping("/{id}/images/soft")
    public ResponseEntity<?> softDeleteSupplierImagesBulk(
            @PathVariable UUID id,
            @RequestParam List<String> filenames
    ) throws IOException {
        return supplierImageService.softDeleteSupplierImagesBulk(id, filenames);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER')")
    @DeleteMapping("/{id}/images/{filename}/hard")
    public ResponseEntity<?> deleteSupplierImage(@PathVariable UUID id, @PathVariable String filename)
            throws IOException {
        return supplierImageService.deleteSupplierImage(id, filename);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER')")
    @DeleteMapping("/{id}/images/hard")
    public ResponseEntity<?> deleteSupplierImagesBulk(
            @PathVariable UUID id,
            @RequestParam List<String> filenames
    ) throws IOException {
        return supplierImageService.deleteSupplierImagesBulk(id, filenames);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PatchMapping("/{id}/images/{filename}/restore")
    @Operation(summary = "Restore a soft-deleted supplier image")
    public ResponseEntity<?> restoreSupplierImage(@PathVariable UUID id, @PathVariable String filename) {
        return supplierImageService.restoreSupplierImage(id, filename);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PatchMapping("/{id}/images/restore")
    @Operation(summary = "Restore multiple soft-deleted supplier images")
    public ResponseEntity<?> restoreSupplierImagesBulk(
            @PathVariable UUID id,
            @RequestParam List<String> filenames
    ) {
        return supplierImageService.restoreSupplierImagesBulk(id, filenames);
    }






    /* ====================== DELETE  ====================== */

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @DeleteMapping("/{id}/soft")
    @Operation(summary = "Soft delete a supplier (mark as inactive)")
    public ResponseEntity<?> softDelete(@PathVariable UUID id) {
        return supplierService.softDeleteSupplier(id);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @DeleteMapping("/bulk/soft")
    @Operation(summary = "Soft delete multiple suppliers (mark as inactive)")
    public ResponseEntity<?> softDeleteInBulk(@RequestBody List<UUID> supplierIds) {
        return supplierService.softDeleteSuppliersInBulk(supplierIds);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PatchMapping("/restore/{id}")
    @Operation(summary = "Restore a soft-deleted supplier")
    public ResponseEntity<?> restore(@PathVariable UUID id) {
        return supplierService.restoreSupplier(id);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PatchMapping("/restore/bulk")
    @Operation(summary = "Restore soft-deleted suppliers")
    public ResponseEntity<?> restoreInBulk(@RequestBody List<UUID> supplierIds) {
        return supplierService.restoreSuppliersInBulk(supplierIds);
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @DeleteMapping("/{id}/hard")
    @Operation(summary = "Permanently delete a supplier and its data")
    public ResponseEntity<?> hardDelete(@PathVariable UUID id) {
        return supplierService.hardDeleteSupplier(id);
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @DeleteMapping("/bulk/hard")
    @Operation(summary = "Permanently delete multiple suppliers and their data")
    public ResponseEntity<?> hardDeleteInBulk(@RequestBody List<UUID> supplierIds) {
        return supplierService.hardDeleteSuppliersInBulk(supplierIds);
    }








    /* ====================== AUDIT ====================== */

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @GetMapping(value = "/{identifier}/images/audit", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get supplier image audit history")
    public ResponseEntity<List<SupplierImageAudit>> getIndividualSupplierImageAudit(@PathVariable String identifier) {
        return supplierImageService.getIndividualSupplierImageAudit(identifier, null);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @GetMapping(value = "/all/images/audit", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get all the suppliers' images audit history")
    public ResponseEntity<List<SupplierImageAudit>> getAllSuppliersImagesAudits() {
        return supplierImageService.getAllSuppliersImagesAudits();
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @GetMapping(value = "/{identifier}/audit", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get supplier audit history")
    public ResponseEntity<List<SupplierAudit>> getIndividualSupplierAudit(@PathVariable String identifier) {
        return supplierService.getIndividualSupplierAudit(identifier, null);

    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @GetMapping(value = "/all/audit", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get all the suppliers' audit history")
    public ResponseEntity<List<SupplierAudit>> getAllSuppliersAudits() {
        return supplierService.getAllSuppliersAudits();

    }
}