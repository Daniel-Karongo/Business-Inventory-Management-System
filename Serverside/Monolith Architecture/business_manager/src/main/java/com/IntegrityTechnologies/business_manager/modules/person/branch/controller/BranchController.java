package com.IntegrityTechnologies.business_manager.modules.person.branch.controller;

import com.IntegrityTechnologies.business_manager.config.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.config.response.PageWrapper;
import com.IntegrityTechnologies.business_manager.modules.person.branch.dto.*;
import com.IntegrityTechnologies.business_manager.modules.person.branch.service.BranchAuditService;
import com.IntegrityTechnologies.business_manager.modules.person.branch.service.BranchBulkService;
import com.IntegrityTechnologies.business_manager.modules.person.branch.service.BranchService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantManagerOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantUserOnly;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

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
       DELETE
    ===================================================== */

    @TenantManagerOnly
    @DeleteMapping("/{id}")
    public ResponseEntity<ApiResponse> deleteBranch(
            @PathVariable UUID id,
            @RequestParam Boolean soft,
            Authentication authentication
    ) {

        branchService.deleteBranch(
                id,
                soft,
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
            @RequestParam Boolean soft,
            Authentication authentication
    ) {

        for (UUID id : ids) {

            branchService.deleteBranch(
                    id,
                    soft,
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