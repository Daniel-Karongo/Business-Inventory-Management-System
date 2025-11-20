package com.IntegrityTechnologies.business_manager.modules.supplier.controller;

import com.IntegrityTechnologies.business_manager.common.PageWrapper;
import com.IntegrityTechnologies.business_manager.modules.supplier.dto.*;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierAudit;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierBulkWithFilesDTO;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierImageAudit;
import com.IntegrityTechnologies.business_manager.modules.supplier.repository.SupplierImageAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.supplier.repository.SupplierAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.supplier.service.SupplierImageService;
import com.IntegrityTechnologies.business_manager.modules.supplier.service.SupplierService;
import com.IntegrityTechnologies.business_manager.modules.user.dto.UserDTO;
import com.IntegrityTechnologies.business_manager.modules.user.model.UserBulkWithFilesDTO;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
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
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
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
            @RequestParam("newImages") @Valid Set<MultipartFile> newImages,
            Authentication authentication
    ) throws IOException {
        String updaterUsername = (authentication != null && authentication.getPrincipal() instanceof UserDetails userDetails)
                ? userDetails.getUsername()
                : null;
        SupplierDTO updatedSupplier = supplierImageService.updateSupplierImages(id, newImages, updaterUsername);
        return ResponseEntity.ok(updatedSupplier);
    }







    /* ====================== READ OPERATIONS ====================== */

    //    All Suppliers
    @GetMapping(value = "/all", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get all suppliers")
    public ResponseEntity<List<SupplierDTO>> getAllSuppliers() {
        return ResponseEntity.ok(supplierService.getAllSuppliers());
    }

    @GetMapping(value = "/all/active", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get all active suppliers")
    public ResponseEntity<List<SupplierDTO>> getAllActiveSuppliers() {
        return ResponseEntity.ok(supplierService.getAllActiveSuppliers());
    }

    @GetMapping(value = "/all/deleted", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get all deleted suppliers")
    public ResponseEntity<List<SupplierDTO>> getAllDeletedSuppliers() {
        return ResponseEntity.ok(supplierService.getAllDeletedSuppliers());
    }

    //    IndividuaL Supplier
    @GetMapping(value = "/identifier/{identifier}/active", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<SupplierDTO> getActiveByIdentifier(@PathVariable String identifier) {
        return ResponseEntity.ok(supplierService.getByIdentifier(identifier, false));
    }

    @GetMapping(value = "/identifier/{identifier}/deleted", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<SupplierDTO> getDeletedByIdentifier(@PathVariable String identifier) {
        return ResponseEntity.ok(supplierService.getByIdentifier(identifier, true));
    }

    @GetMapping(value = "/identifier/{identifier}/any", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Find ANY supplier by name, email, or phone")
    public ResponseEntity<SupplierDTO> getActiveOrDeletedByIdentifier(@PathVariable String identifier) {
        return ResponseEntity.ok(supplierService.getByIdentifier(identifier, null));
    }

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
            @RequestParam(defaultValue = "asc") String direction
    ) {
        var result = supplierService.advancedSearch(
                categoryIds, name, email, phone, region,
                minRating, createdAfter, createdBefore,
                page, size, sortBy, direction
        );
        return ResponseEntity.ok(new PageWrapper<>(result));
    }








    /* ====================== IMAGE MANAGEMENT ====================== */

    /* ====================== IMAGE URLS ====================== */
    @GetMapping(value = "/{identifier}/images/all", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get all image URLs for a supplier")
    public ResponseEntity<List<String>> getAllSupplierImages(@PathVariable String id) {
        return ResponseEntity.ok(supplierImageService.getSupplierImageUrls(id, null));
    }

    @GetMapping(value = "/{identifier}/images/soft-deleted", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get all image URLs for a supplier")
    public ResponseEntity<List<String>> getActiveSupplierImages(@PathVariable String id) {
        return ResponseEntity.ok(supplierImageService.getSupplierImageUrls(id, false));
    }

    @GetMapping(value = "/{identifier}/images/deleted", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get all image URLs for a supplier")
    public ResponseEntity<List<String>> getDeletedSupplierImages(@PathVariable String id) {
        return ResponseEntity.ok(supplierImageService.getSupplierImageUrls(id, true));
    }




    /* ====================== IMAGES DOWNLOADS ====================== */

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @GetMapping("/images/all/active")
    public ResponseEntity<List<String>> getAllActiveImages() {
        return ResponseEntity.ok(supplierImageService.getAllSuppliersImages(false));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @GetMapping("/images/all/soft-deleted")
    public ResponseEntity<List<String>> getAllSoftDeletedImages() {
        return ResponseEntity.ok(supplierImageService.getAllSuppliersImages(true));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @GetMapping("/images/all/any")
    public ResponseEntity<List<String>> getAllImages() {
        return ResponseEntity.ok(supplierImageService.getAllSuppliersImages(null));
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @GetMapping("/images/all/download/active")
    public ResponseEntity<Resource> downloadAllActiveImages() throws IOException {
        return supplierImageService.downloadAllSuppliersImages(false);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @GetMapping("/images/all/download/soft-deleted")
    public ResponseEntity<Resource> downloadAllSoftDeletedImages() throws IOException {
        return supplierImageService.downloadAllSuppliersImages(true);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    @GetMapping("/images/all/download/all")
    public ResponseEntity<Resource> downloadAllImages() throws IOException {
        return supplierImageService.downloadAllSuppliersImages(null);
    }




    @GetMapping("/{identifier}/images/{filename}/active")
    @Operation(summary = "Download a specific supplier image")
    public ResponseEntity<Resource> downloadSupplierActiveImage(@PathVariable String id, @PathVariable String filename) throws IOException {
        Resource file = supplierImageService.downloadImage(id, filename, false);
        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + filename + "\"")
                .body(file);
    }

    @GetMapping("/{identifier}/images/{filename}/soft-deleted")
    @Operation(summary = "Download a specific supplier image")
    public ResponseEntity<Resource> downloadSupplierSoftDeletedImage(@PathVariable String id, @PathVariable String filename) throws IOException {
        Resource file = supplierImageService.downloadImage(id, filename, true);
        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + filename + "\"")
                .body(file);
    }

    @GetMapping("/{identifier}/images/{filename}/any")
    @Operation(summary = "Download a specific supplier image")
    public ResponseEntity<Resource> downloadSupplierAnyImage(@PathVariable String id, @PathVariable String filename) throws IOException {
        Resource file = supplierImageService.downloadImage(id, filename, null);
        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + filename + "\"")
                .body(file);
    }

    @GetMapping("/{identifier}/images/zip/all")
    @Operation(summary = "Download all supplier images as a ZIP")
    public void downloadSupplierAllImagesZip(@PathVariable String id, HttpServletResponse response) throws IOException {
        response.setContentType("application/zip");
        response.setHeader(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"supplier_" + id + "_images.zip\"");
        supplierImageService.streamAllImagesAsZip(id, response.getOutputStream(), null);
    }

    @GetMapping("/{identifier}/images/zip/active")
    @Operation(summary = "Download all supplier images as a ZIP")
    public void downloadSupplierAllActiveImagesZip(@PathVariable String id, HttpServletResponse response) throws IOException {
        response.setContentType("application/zip");
        response.setHeader(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"supplier_" + id + "_images.zip\"");
        supplierImageService.streamAllImagesAsZip(id, response.getOutputStream(), false);
    }

    @GetMapping("/{identifier}/images/zip/deleted")
    @Operation(summary = "Download all supplier images as a ZIP")
    public void downloadSupplierAllDeletedImagesZip(@PathVariable String id, HttpServletResponse response) throws IOException {
        response.setContentType("application/zip");
        response.setHeader(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"supplier_" + id + "_images.zip\"");
        supplierImageService.streamAllImagesAsZip(id, response.getOutputStream(), false);
    }





    @DeleteMapping("/{id}/images/{filename}/soft")
    public ResponseEntity<?> softDeleteSupplierImage(@PathVariable UUID id, @PathVariable String filename)
            throws IOException {
        return supplierImageService.softDeleteSupplierImage(id, filename);
    }

    @DeleteMapping("/{id}/images/soft")
    public ResponseEntity<?> softDeleteSupplierImagesBulk(
            @PathVariable UUID id,
            @RequestParam List<String> filenames
    ) throws IOException {
        return supplierImageService.softDeleteSupplierImagesBulk(id, filenames);
    }

    @DeleteMapping("/{id}/images/{filename}/hard")
    public ResponseEntity<?> deleteSupplierImage(@PathVariable UUID id, @PathVariable String filename)
            throws IOException {
        return supplierImageService.deleteSupplierImage(id, filename);
    }

    @DeleteMapping("/{id}/images/hard")
    public ResponseEntity<?> deleteSupplierImagesBulk(
            @PathVariable UUID id,
            @RequestParam List<String> filenames
    ) throws IOException {
        return supplierImageService.deleteSupplierImagesBulk(id, filenames);
    }

    @PatchMapping("/{id}/images/{filename}/restore")
    @Operation(summary = "Restore a soft-deleted supplier image")
    public ResponseEntity<?> restoreSupplierImage(@PathVariable UUID id, @PathVariable String filename) {
        return supplierImageService.restoreSupplierImage(id, filename);
    }

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

    @PreAuthorize("hasRole('SUPERUSER')")
    @PatchMapping("/restore/{id}")
    @Operation(summary = "Restore a soft-deleted supplier")
    public ResponseEntity<?> restore(@PathVariable UUID id) {
        return supplierService.restoreSupplier(id);
    }

    @PreAuthorize("hasRole('SUPERUSER')")
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

    @GetMapping(value = "/{id}/images/audit", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get supplier image audit history")
    public ResponseEntity<List<SupplierImageAudit>> getSupplierImageAudit(@PathVariable UUID id) {
        return ResponseEntity.ok(supplierImageAuditRepository.findBySupplierId(id));
    }

    @GetMapping(value = "/{id}/audit", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get supplier audit history")
    public ResponseEntity<List<SupplierAudit>> getSupplierAudit(@PathVariable UUID id) {
        return ResponseEntity.ok(supplierAuditRepository.findBySupplierIdOrderByTimestampDesc(id));
    }
}