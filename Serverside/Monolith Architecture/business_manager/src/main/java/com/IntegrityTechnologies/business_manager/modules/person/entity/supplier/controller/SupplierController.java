package com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.controller;

import com.IntegrityTechnologies.business_manager.config.files.FIleUploadDTO;
import com.IntegrityTechnologies.business_manager.config.files.FileSecurityUtil;
import com.IntegrityTechnologies.business_manager.config.response.PageWrapper;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.dto.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.model.*;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.repository.SupplierImageAuditRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.service.SupplierBulkService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.service.SupplierImageService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.supplier.service.SupplierService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.*;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.http.*;
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
@TenantUserOnly
public class SupplierController {

    private final SupplierService supplierService;
    private final SupplierImageAuditRepository supplierImageAuditRepository;
    private final SupplierAuditRepository supplierAuditRepository;
    private final SupplierImageService supplierImageService;
    private final SupplierBulkService bulkService;

    /* ====================================
       IMPORT
       ==================================== */

    @TenantManagerOnly
    @PostMapping("/import")
    public ResponseEntity<BulkResult<SupplierDTO>> importSuppliers(
            @RequestBody BulkRequest<SupplierBulkRow> request,
            Authentication authentication
    ) {

        String creatorUsername =
                (authentication != null && authentication.getPrincipal() instanceof UserDetails ud)
                        ? ud.getUsername()
                        : null;

        return ResponseEntity.ok(
                bulkService.importSuppliers(request, creatorUsername)
        );
    }

    /* ====================================
       CREATE
       ==================================== */

    @TenantManagerOnly
    @PostMapping(
            value = "/register",
            consumes = MediaType.MULTIPART_FORM_DATA_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE
    )
    public ResponseEntity<SupplierDTO> registerSupplier(
            @ModelAttribute("supplierDTO") @Valid SupplierCreateDTO supplierDTO,
            Authentication authentication
    ) throws IOException {

        String creatorUsername =
                (authentication != null && authentication.getPrincipal() instanceof UserDetails userDetails)
                        ? userDetails.getUsername()
                        : null;

        SupplierDTO created =
                supplierService.createSupplier(supplierDTO, creatorUsername);

        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(created);
    }

    /* ====================================
       UPDATE
       ==================================== */

    @TenantManagerOnly
    @PatchMapping("/{id}")
    public ResponseEntity<SupplierDTO> updateSupplier(
            @PathVariable UUID id,
            @RequestBody @Valid SupplierUpdateDTO updated,
            Authentication authentication
    ) throws IOException {

        String updaterUsername =
                (authentication != null && authentication.getPrincipal() instanceof UserDetails userDetails)
                        ? userDetails.getUsername()
                        : null;

        SupplierDTO updatedSupplier =
                supplierService.updateSupplier(id, updated, updaterUsername);

        return ResponseEntity.ok(updatedSupplier);
    }

    @TenantManagerOnly
    @PatchMapping(value = "/{id}/images", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<SupplierDTO> updateSupplierImages(
            @PathVariable UUID id,
            @RequestPart("files") List<MultipartFile> files,
            @RequestPart(value = "descriptions", required = false) List<String> descriptions,
            Authentication authentication
    ) throws IOException {

        String updaterUsername =
                (authentication != null && authentication.getPrincipal() instanceof UserDetails userDetails)
                        ? userDetails.getUsername()
                        : null;

        List<FIleUploadDTO> dtos = new ArrayList<>();

        for (int i = 0; i < files.size(); i++) {
            dtos.add(new FIleUploadDTO(
                    files.get(i),
                    descriptions != null && i < descriptions.size() ? descriptions.get(i) : null
            ));
        }

        SupplierDTO updatedSupplier =
                supplierImageService.updateSupplierImages(id, dtos, updaterUsername);

        return ResponseEntity.ok(updatedSupplier);
    }

    /* ====================================
       READ
       ==================================== */

    @TenantSupervisorOnly
    @GetMapping(value = "/all", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get suppliers (active, deleted, or any)")
    public ResponseEntity<List<SupplierDTO>> getSuppliers(
            @RequestParam(required = false) Boolean deleted
    ) {

        return ResponseEntity.ok(
                supplierService.getSuppliers(deleted)
        );
    }

    @TenantSupervisorOnly
    @GetMapping(value = "/identifier/{identifier}", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Find supplier by UUID, name, email, or phone")
    public ResponseEntity<SupplierDTO> getSupplierByIdentifier(
            @PathVariable String identifier,
            @RequestParam(required = false) Boolean deleted
    ) {

        return ResponseEntity.ok(
                supplierService.getByIdentifier(identifier, deleted)
        );
    }

    @TenantSupervisorOnly
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

        var result =
                supplierService.advancedSearch(
                        categoryIds,
                        name,
                        email,
                        phone,
                        region,
                        minRating,
                        createdAfter,
                        createdBefore,
                        page,
                        size,
                        sortBy,
                        direction,
                        deleted
                );

        return ResponseEntity.ok(
                new PageWrapper<>(result)
        );
    }

    /* ====================================
       IMAGE MANAGEMENT
       ==================================== */

    @TenantManagerOnly
    @GetMapping("/{id}/images")
    public ResponseEntity<List<SupplierImageDTO>> listSupplierImages(
            @PathVariable UUID id,
            @RequestParam(required = false) Boolean deleted
    ) {

        return ResponseEntity.ok(
                supplierImageService.listImages(id, deleted)
        );
    }

    @TenantManagerOnly
    @GetMapping("/images/all")
    public ResponseEntity<List<String>> getAllImages(
            @RequestParam(required = false) Boolean deletedSupplier,
            @RequestParam(required = false) Boolean deletedImage
    ) {

        return ResponseEntity.ok(
                supplierImageService.getAllSuppliersImages(deletedSupplier, deletedImage)
        );
    }

    /* ====================================
       IMAGE DOWNLOAD
       ==================================== */
    @TenantManagerOnly
    @GetMapping("/{id}/images/{filename}")
    public ResponseEntity<Resource> downloadSupplierImage(
            @PathVariable UUID id,
            @PathVariable String filename,
            @RequestParam(required = false) Boolean deleted
    ) throws IOException {

        filename = FileSecurityUtil.sanitizeFilename(filename);

        Resource file =
                supplierImageService.downloadImage(id, filename, deleted);

        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .header(HttpHeaders.CONTENT_DISPOSITION,
                        "attachment; filename=\"" + filename + "\"")
                .body(file);
    }

    @TenantManagerOnly
    @GetMapping("/{id}/images/zip")
    public void downloadSupplierImagesZip(
            @PathVariable UUID id,
            @RequestParam(required = false) Boolean deleted,
            HttpServletResponse response
    ) throws IOException {

        List<SupplierImage> images =
                supplierImageService.getImages(id, deleted);

        Supplier supplier =
                supplierImageService.getSupplier(id);

        response.setContentType("application/zip");

        String state =
                deleted == null
                        ? "all"
                        : deleted
                        ? "deleted"
                        : "active";

        response.setHeader(
                HttpHeaders.CONTENT_DISPOSITION,
                "attachment; filename=\"supplier_" + id + "_" + state + "_images.zip\""
        );

        supplierImageService.writeImagesToZip(
                images,
                supplier,
                response.getOutputStream()
        );
    }

    @TenantManagerOnly
    @GetMapping("/images/all/download")
    public ResponseEntity<Resource> downloadAllImages(
            @RequestParam(required = false) Boolean deletedSupplier,
            @RequestParam(required = false) Boolean deletedImage
    ) throws IOException {

        return supplierImageService.downloadAllSuppliersImages(
                deletedSupplier,
                deletedImage
        );
    }

    /* ====================================
       IMAGE DELETE
       ==================================== */

    @TenantManagerOnly
    @DeleteMapping("/{id}/images/{filename}/soft")
    public ResponseEntity<?> softDeleteSupplierImage(
            @PathVariable UUID id,
            @PathVariable String filename
    ) throws IOException {

        filename = FileSecurityUtil.sanitizeFilename(filename);

        return supplierImageService.softDeleteSupplierImage(id, filename);
    }

    @TenantManagerOnly
    @DeleteMapping("/{id}/images/soft")
    public ResponseEntity<?> softDeleteSupplierImagesBulk(
            @PathVariable UUID id,
            @RequestParam List<String> filenames
    ) throws IOException {

        return supplierImageService.softDeleteSupplierImagesBulk(id, filenames);
    }

    @PlatformAdminOnly
    @DeleteMapping("/{id}/images/{filename}/hard")
    public ResponseEntity<?> deleteSupplierImage(
            @PathVariable UUID id,
            @PathVariable String filename
    ) throws IOException {

        filename = FileSecurityUtil.sanitizeFilename(filename);

        return supplierImageService.deleteSupplierImage(id, filename);
    }

    @PlatformAdminOnly
    @DeleteMapping("/{id}/images/hard")
    public ResponseEntity<?> deleteSupplierImagesBulk(
            @PathVariable UUID id,
            @RequestParam List<String> filenames
    ) throws IOException {

        return supplierImageService.deleteSupplierImagesBulk(id, filenames);
    }

    @TenantManagerOnly
    @PatchMapping("/{id}/images/{filename}/restore")
    public ResponseEntity<?> restoreSupplierImage(
            @PathVariable UUID id,
            @PathVariable String filename
    ) {

        filename = FileSecurityUtil.sanitizeFilename(filename);

        return supplierImageService.restoreSupplierImage(id, filename);
    }

    @TenantManagerOnly
    @PatchMapping("/{id}/images/restore")
    public ResponseEntity<?> restoreSupplierImagesBulk(
            @PathVariable UUID id,
            @RequestParam List<String> filenames
    ) {

        return supplierImageService.restoreSupplierImagesBulk(id, filenames);
    }

    /* ====================================
       DELETE
       ==================================== */

    @TenantManagerOnly
    @DeleteMapping("/{id}/soft")
    public ResponseEntity<?> softDelete(
            @PathVariable UUID id
    ) {

        return supplierService.softDeleteSupplier(id);
    }

    @TenantManagerOnly
    @DeleteMapping("/bulk/soft")
    public ResponseEntity<?> softDeleteInBulk(
            @RequestBody List<UUID> supplierIds
    ) {

        return supplierService.softDeleteSuppliersInBulk(supplierIds);
    }

    @TenantManagerOnly
    @PatchMapping("/restore/{id}")
    public ResponseEntity<?> restore(
            @PathVariable UUID id
    ) {

        return supplierService.restoreSupplier(id);
    }

    @TenantManagerOnly
    @PatchMapping("/restore/bulk")
    public ResponseEntity<?> restoreInBulk(
            @RequestBody List<UUID> supplierIds
    ) {

        return supplierService.restoreSuppliersInBulk(supplierIds);
    }

    @PlatformAdminOnly
    @DeleteMapping("/{id}/hard")
    public ResponseEntity<?> hardDelete(
            @PathVariable UUID id
    ) {

        return supplierService.hardDeleteSupplier(id);
    }

    @PlatformAdminOnly
    @DeleteMapping("/bulk/hard")
    public ResponseEntity<?> hardDeleteInBulk(
            @RequestBody List<UUID> supplierIds
    ) {

        return supplierService.hardDeleteSuppliersInBulk(supplierIds);
    }

    /* ====================================
       AUDIT
       ==================================== */

    @TenantManagerOnly
    @GetMapping("/{identifier}/images/audit")
    public ResponseEntity<List<SupplierImageAudit>> getIndividualSupplierImageAudit(
            @PathVariable String identifier
    ) {

        return supplierImageService.getIndividualSupplierImageAudit(identifier, null);
    }

    @TenantManagerOnly
    @GetMapping("/all/images/audit")
    public ResponseEntity<List<SupplierImageAudit>> getAllSuppliersImagesAudits() {

        return supplierImageService.getAllSuppliersImagesAudits();
    }

    @TenantManagerOnly
    @GetMapping("/{identifier}/audit")
    public ResponseEntity<List<SupplierAudit>> getIndividualSupplierAudit(
            @PathVariable String identifier
    ) {

        return supplierService.getIndividualSupplierAudit(identifier, null);
    }

    @TenantManagerOnly
    @GetMapping("/all/audit")
    public ResponseEntity<List<SupplierAudit>> getAllSuppliersAudits() {

        return supplierService.getAllSuppliersAudits();
    }
}