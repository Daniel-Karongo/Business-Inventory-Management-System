package com.IntegrityTechnologies.business_manager.modules.person.branch.controller;

import com.IntegrityTechnologies.business_manager.config.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.config.response.PageWrapper;
import com.IntegrityTechnologies.business_manager.modules.person.branch.deletion.BranchDeletionMode;
import com.IntegrityTechnologies.business_manager.modules.person.branch.document.dto.BranchDocumentAuditDTO;
import com.IntegrityTechnologies.business_manager.modules.person.branch.document.dto.BranchDocumentDTO;
import com.IntegrityTechnologies.business_manager.modules.person.branch.document.dto.BranchDocumentsUploadForm;
import com.IntegrityTechnologies.business_manager.modules.person.branch.document.dto.UpdateDocumentDescriptionDTO;
import com.IntegrityTechnologies.business_manager.modules.person.branch.document.service.BranchDocumentService;
import com.IntegrityTechnologies.business_manager.modules.person.branch.dto.*;
import com.IntegrityTechnologies.business_manager.modules.person.branch.service.BranchAuditService;
import com.IntegrityTechnologies.business_manager.modules.person.branch.service.BranchBulkService;
import com.IntegrityTechnologies.business_manager.modules.person.branch.service.BranchService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantSuperuserOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/branches")
@RequiredArgsConstructor
@TenantUserOnly
public class BranchController {

    private final BranchService branchService;
    private final BranchBulkService bulkService;
    private final BranchAuditService branchAuditService;
    private final BranchDocumentService branchDocumentService;

    /* =====================================================
       IMPORT
    ===================================================== */

    @TenantManagerOnly
    @PostMapping("/import")
    public ResponseEntity<BulkResult<BranchDetailsDTO>> importBranches(
            @RequestBody BulkRequest<BranchBulkRow> request,
            Authentication authentication
    ) {

        return ResponseEntity.ok(
                bulkService.importBranches(
                        request,
                        authentication
                )
        );
    }

    /* =====================================================
       CREATE
    ===================================================== */

    @TenantAdminOnly
    @PostMapping
    public ResponseEntity<ApiResponse> create(
            @RequestBody BranchFormDTO request,
            Authentication authentication
    ) {

        return ResponseEntity.ok(
                new ApiResponse(
                        "success",
                        "Branch created successfully",
                        branchService.create(
                                request,
                                authentication
                        )
                )
        );
    }

    /* =====================================================
       READ
    ===================================================== */

    @GetMapping
    public ResponseEntity<PageWrapper<BranchListItemDTO>> getAll(
            @ModelAttribute BranchQueryDTO query
    ) {

        return ResponseEntity.ok(
                branchService.getAll(query)
        );
    }

    @GetMapping("/{id}")
    public ResponseEntity<BranchDetailsDTO> getById(
            @PathVariable UUID id
    ) {

        return ResponseEntity.ok(
                branchService.getById(id)
        );
    }

    /* =====================================================
       AUDITS
    ===================================================== */

    @GetMapping("/{id}/audits")
    public ResponseEntity<PageWrapper<BranchAuditDTO>> getAudits(
            @PathVariable UUID id,

            @RequestParam(defaultValue = "0")
            int page,

            @RequestParam(defaultValue = "20")
            int size
    ) {

        return ResponseEntity.ok(
                branchAuditService.getAudits(
                        id,
                        page,
                        size
                )
        );
    }

    /* =====================================================
       UPDATE
    ===================================================== */

    @TenantManagerOnly
    @PatchMapping("/{id}")
    public ResponseEntity<BranchDetailsDTO> update(
            @PathVariable UUID id,
            @RequestBody BranchFormDTO request,
            Authentication authentication
    ) {

        return ResponseEntity.ok(
                branchService.update(
                        id,
                        request,
                        authentication
                )
        );
    }

        /* =====================================================
       ATTENDANCE SETTINGS
    ===================================================== */

    @TenantManagerOnly
    @PatchMapping("/{id}/attendance-settings")
    public ResponseEntity<BranchDetailsDTO> updateAttendanceSettings(
            @PathVariable UUID id,
            @Valid @RequestBody BranchAttendanceSettingsUpdateDTO request,
            Authentication authentication
    ) {
        return ResponseEntity.ok(
                branchService.updateAttendanceSettings(
                        id,
                        request,
                        authentication
                )
        );
    }

    /* =====================================================
       SECURITY SETTINGS
    ===================================================== */

    @TenantManagerOnly
    @PatchMapping("/{id}/security-settings")
    public ResponseEntity<BranchDetailsDTO> updateSecuritySettings(
            @PathVariable UUID id,
            @Valid @RequestBody BranchSecuritySettingsUpdateDTO request,
            Authentication authentication
    ) {
        return ResponseEntity.ok(
                branchService.updateSecuritySettings(
                        id,
                        request,
                        authentication
                )
        );
    }

    /* =====================================================
       DOCUMENTS
    ===================================================== */

    @TenantManagerOnly
    @PatchMapping(
            value = "/{branchId}/documents",
            consumes = MediaType.MULTIPART_FORM_DATA_VALUE
    )
    public ResponseEntity<?> uploadDocuments(
            @PathVariable UUID branchId,
            @ModelAttribute BranchDocumentsUploadForm form,
            Authentication authentication
    ) throws IOException {

        branchDocumentService.uploadDocuments(
                branchId,
                form.getFiles(),
                authentication
        );

        return ResponseEntity.ok().build();
    }

    @TenantManagerOnly
    @PatchMapping("/{branchId}/documents/{filename:.+}/description")
    public ResponseEntity<?> updateDescription(
            @PathVariable UUID branchId,
            @PathVariable String filename,
            @RequestBody UpdateDocumentDescriptionDTO request,
            Authentication authentication
    ) {
        branchDocumentService.updateDescription(
                branchId,
                filename,
                request.getDescription(),
                authentication
        );

        return ResponseEntity.ok().build();
    }

    @TenantUserOnly
    @GetMapping("/{branchId}/documents")
    public ResponseEntity<List<BranchDocumentDTO>> getDocuments(
            @PathVariable UUID branchId,
            @RequestParam(required = false) Boolean deleted
    ) {

        return ResponseEntity.ok(
                branchDocumentService.getDocuments(
                        branchId,
                        deleted
                )
        );
    }

    @TenantUserOnly
    @GetMapping("/{branchId}/documents/{filename:.+}")
    public ResponseEntity<Resource> downloadDocument(
            @PathVariable UUID branchId,
            @PathVariable String filename,
            Authentication authentication
    ) throws IOException {

        return branchDocumentService.download(
                branchId,
                filename,
                authentication
        );
    }

    @TenantManagerOnly
    @PatchMapping("/{branchId}/documents/{filename:.+}/logo")
    public ResponseEntity<?> setLogo(
            @PathVariable UUID branchId,
            @PathVariable String filename,
            Authentication authentication
    ) {

        branchDocumentService.setLogo(
                branchId,
                filename,
                authentication
        );

        return ResponseEntity.ok().build();
    }

    @TenantManagerOnly
    @DeleteMapping("/{branchId}/documents/{filename:.+}/soft")
    public ResponseEntity<?> softDeleteDocument(
            @PathVariable UUID branchId,
            @PathVariable String filename,
            @RequestBody(required = false) String reason,
            Authentication authentication
    ) {

        branchDocumentService.softDelete(
                branchId,
                filename,
                reason,
                authentication
        );

        return ResponseEntity.ok().build();
    }

    @TenantManagerOnly
    @PatchMapping("/{branchId}/documents/{filename:.+}/restore")
    public ResponseEntity<?> restoreDocument(
            @PathVariable UUID branchId,
            @PathVariable String filename,
            @RequestBody(required = false) String reason,
            Authentication authentication
    ) {

        branchDocumentService.restore(
                branchId,
                filename,
                reason,
                authentication
        );

        return ResponseEntity.ok().build();
    }

    @TenantSuperuserOnly
    @DeleteMapping("/{branchId}/documents/{filename:.+}/hard")
    public ResponseEntity<?> hardDeleteDocument(
            @PathVariable UUID branchId,
            @PathVariable String filename,
            @RequestBody(required = false) String reason,
            Authentication authentication
    ) {

        branchDocumentService.hardDelete(
                branchId,
                filename,
                reason,
                authentication
        );

        return ResponseEntity.ok().build();
    }

    @TenantManagerOnly
    @DeleteMapping("/{branchId}/documents/soft")
    public ResponseEntity<?> softDeleteAllDocuments(
            @PathVariable UUID branchId,
            Authentication authentication
    ) {

        branchDocumentService.softDeleteAll(
                branchId,
                authentication
        );

        return ResponseEntity.ok().build();
    }

    @TenantManagerOnly
    @PatchMapping("/{branchId}/documents/restore")
    public ResponseEntity<?> restoreAllDocuments(
            @PathVariable UUID branchId,
            Authentication authentication
    ) {

        branchDocumentService.restoreAll(
                branchId,
                authentication
        );

        return ResponseEntity.ok().build();
    }

    @TenantSuperuserOnly
    @DeleteMapping("/{branchId}/documents/hard")
    public ResponseEntity<?> hardDeleteAllDocuments(
            @PathVariable UUID branchId,
            Authentication authentication
    ) {

        branchDocumentService.hardDeleteAll(
                branchId,
                authentication
        );

        return ResponseEntity.ok().build();
    }

    @TenantManagerOnly
    @GetMapping("/{branchId}/documents/audits")
    public ResponseEntity<List<BranchDocumentAuditDTO>> audits(
            @PathVariable UUID branchId
    ) {

        return ResponseEntity.ok(
                branchDocumentService.getBranchDocumentAudits(
                        branchId
                )
        );
    }

    /* =====================================================
       DELETE
    ===================================================== */

    @TenantManagerOnly
    @DeleteMapping("/{id}")
    public ResponseEntity<ApiResponse> deleteBranch(
            @PathVariable UUID id,
            @RequestParam BranchDeletionMode mode,
            Authentication authentication
    ) {

        branchService.deleteBranch(
                id,
                mode,
                authentication
        );

        return ResponseEntity.ok(
                new ApiResponse(
                        "success",
                        "Branch deleted successfully"
                )
        );
    }

    @TenantManagerOnly
    @DeleteMapping("/bulk")
    public ResponseEntity<ApiResponse> deleteBulk(
            @RequestBody List<UUID> ids,
            @RequestParam BranchDeletionMode mode,
            Authentication authentication
    ) {

        for (UUID id : ids) {

            branchService.deleteBranch(
                    id,
                    mode,
                    authentication
            );
        }

        return ResponseEntity.ok(
                new ApiResponse(
                        "success",
                        "Branches deleted successfully"
                )
        );
    }

    /* =====================================================
       RESTORE
    ===================================================== */

    @TenantManagerOnly
    @PatchMapping("/restore/{id}")
    public ResponseEntity<ApiResponse> restore(
            @PathVariable UUID id,
            Authentication authentication
    ) {

        branchService.restoreBranch(
                id,
                authentication
        );

        return ResponseEntity.ok(
                new ApiResponse(
                        "success",
                        "Branch restored successfully"
                )
        );
    }

    @TenantManagerOnly
    @PatchMapping("/restore/bulk")
    public ResponseEntity<ApiResponse> restoreBulk(
            @RequestBody List<UUID> ids,
            Authentication authentication
    ) {

        for (UUID id : ids) {

            branchService.restoreBranch(
                    id,
                    authentication
            );
        }

        return ResponseEntity.ok(
                new ApiResponse(
                        "success",
                        "Branches restored successfully"
                )
        );
    }
}