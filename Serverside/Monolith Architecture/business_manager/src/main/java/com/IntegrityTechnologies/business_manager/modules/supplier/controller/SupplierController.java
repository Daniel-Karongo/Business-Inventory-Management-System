package com.IntegrityTechnologies.business_manager.modules.supplier.controller;

import com.IntegrityTechnologies.business_manager.common.PageWrapper;
import com.IntegrityTechnologies.business_manager.modules.supplier.dto.*;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierAudit;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierBulkWithFilesDTO;
import com.IntegrityTechnologies.business_manager.modules.supplier.model.SupplierImageAudit;
import com.IntegrityTechnologies.business_manager.modules.supplier.repository.SupplierImageAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.supplier.repository.SupplierAuditRepository;
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
import java.util.UUID;

@Tag(name = "Suppliers")
@RestController
@RequestMapping("/api/suppliers")
@RequiredArgsConstructor
public class SupplierController {

    private final SupplierService supplierService;
    private final SupplierImageAuditRepository supplierImageAuditRepository;
    private final SupplierAuditRepository supplierAuditRepository;

    /* ====================== READ OPERATIONS ====================== */

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

    @GetMapping(value = "/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get supplier by ID")
    public ResponseEntity<SupplierDTO> getSupplier(@PathVariable UUID id) {
        return ResponseEntity.ok(supplierService.getSupplierDto(id));
    }

    @GetMapping(value = "/identifier/{identifier}", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Find supplier by name, email, or phone")
    public ResponseEntity<SupplierDTO> getByIdentifier(@PathVariable String identifier) {
        return ResponseEntity.ok(supplierService.getByIdentifier(identifier));
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

    @GetMapping(value = "/{id}/images", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get all image URLs for a supplier")
    public ResponseEntity<List<String>> getSupplierImages(@PathVariable UUID id) {
        return ResponseEntity.ok(supplierService.getSupplierImageUrls(id));
    }

    @GetMapping("/{id}/images/{filename}")
    @Operation(summary = "Download a specific supplier image")
    public ResponseEntity<Resource> downloadImage(@PathVariable UUID id, @PathVariable String filename) {
        Resource file = supplierService.downloadImage(id, filename);
        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + filename + "\"")
                .body(file);
    }

    @GetMapping("/{id}/images/zip")
    @Operation(summary = "Download all supplier images as a ZIP")
    public void downloadAllImagesZip(@PathVariable UUID id, HttpServletResponse response) throws IOException {
        response.setContentType("application/zip");
        response.setHeader(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"supplier_" + id + "_images.zip\"");
        supplierService.streamAllImagesAsZip(id, response.getOutputStream());
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

    /* ====================== CREATE / UPDATE / DELETE ====================== */

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PostMapping(value="/register", consumes = MediaType.MULTIPART_FORM_DATA_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(
            summary = "Create new supplier with optional images",
            responses = @ApiResponse(responseCode = "201", description = "Supplier created successfully",
                    content = @Content(schema = @Schema(implementation = SupplierDTO.class)))
    )
    public ResponseEntity<SupplierDTO> createSupplier(
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
    @PostMapping(value="/register/bulk")
    public ResponseEntity<List<SupplierDTO>> registerSuppliersInBulk(
            @RequestBody List<SupplierCreateDTO> supplierDTOs,
            Authentication authentication
    ) throws IOException {
        String creatorUsername = (authentication != null && authentication.getPrincipal() instanceof UserDetails userDetails)
                ? userDetails.getUsername()
                : null;

        List<SupplierDTO> savedSupplierDTOs = new ArrayList<>();
        for(SupplierCreateDTO supplierDTO: supplierDTOs) {
            savedSupplierDTOs.add(supplierService.createSupplier(supplierDTO, creatorUsername));
        }

        return ResponseEntity.status(HttpStatus.CREATED).body(savedSupplierDTOs);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @PutMapping("/{id}")
    @Operation(summary = "Update supplier information")
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
    @PutMapping(value="/{id}/images")
    @Operation(summary = "Update supplier images")
    public ResponseEntity<SupplierDTO> updateSupplierImages (
            @PathVariable UUID id,
            @RequestParam("newImages") @Valid  List<MultipartFile> newImages,
            Authentication authentication
    ) throws IOException {
        String updaterUsername = (authentication != null && authentication.getPrincipal() instanceof UserDetails userDetails)
                ? userDetails.getUsername()
                : null;
        SupplierDTO updatedSupplier = supplierService.updateSupplierImages(id, newImages, updaterUsername);
        return ResponseEntity.ok(updatedSupplier);
    }

    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    @DeleteMapping("/soft/{id}")
    @Operation(summary = "Soft delete a supplier (mark as inactive)")
    public ResponseEntity<?> softDelete(@PathVariable UUID id) {
        return supplierService.softDeleteSupplier(id);
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @PatchMapping("/restore/{id}")
    @Operation(summary = "Restore a soft-deleted supplier")
    public ResponseEntity<?> restore(@PathVariable UUID id) {
        return supplierService.restoreSupplier(id);
    }

    @PreAuthorize("hasRole('SUPERUSER')")
    @DeleteMapping("/hard/{id}")
    @Operation(summary = "Permanently delete a supplier and its data")
    public ResponseEntity<?> hardDelete(@PathVariable UUID id) {
        return supplierService.hardDeleteSupplier(id);
    }

    @DeleteMapping("/{id}/images/{filename}")
    @Operation(summary = "Delete a specific supplier image")
    public ResponseEntity<?> deleteSupplierImage(@PathVariable UUID id, @PathVariable String filename)
            throws IOException {
        return supplierService.deleteSupplierImage(id, filename);
    }

    @DeleteMapping("/{id}/images")
    @Operation(summary = "Delete multiple supplier images")
    public ResponseEntity<?> deleteSupplierImagesBulk(
            @PathVariable UUID id,
            @RequestParam List<String> filenames
    ) throws IOException {
        return supplierService.deleteSupplierImagesBulk(id, filenames);
    }
}