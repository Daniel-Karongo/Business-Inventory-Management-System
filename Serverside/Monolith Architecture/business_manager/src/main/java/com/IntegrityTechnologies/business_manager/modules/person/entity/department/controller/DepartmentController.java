package com.IntegrityTechnologies.business_manager.modules.person.entity.department.controller;

import com.IntegrityTechnologies.business_manager.config.response.ApiResponse;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkRequest;
import com.IntegrityTechnologies.business_manager.config.bulk.BulkResult;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto.DepartmentBulkRow;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto.DepartmentDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto.DepartmentMinimalDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.mapper.DepartmentMapper;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.DepartmentAudit;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.service.DepartmentBulkService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.service.DepartmentService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.*;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/departments")
@RequiredArgsConstructor
@Tag(name = "Departments")
@TenantUserOnly
public class DepartmentController {

    private final DepartmentService departmentService;
    private final DepartmentMapper departmentMapper;
    private final DepartmentBulkService bulkService;

    /* ====================================
       IMPORT
       ==================================== */

    @TenantManagerOnly
    @PostMapping("/import")
    public ResponseEntity<BulkResult<DepartmentDTO>> importDepartments(
            @RequestBody BulkRequest<DepartmentBulkRow> request,
            Authentication authentication
    ) {
        return ResponseEntity.ok(
                bulkService.importDepartments(request, authentication)
        );
    }

    /* ====================================
       CREATE
       ==================================== */

    @TenantManagerOnly
    @PostMapping
    public ResponseEntity<DepartmentDTO> create(
            @RequestBody DepartmentDTO dto,
            Authentication authentication
    ) {
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(departmentService.create(dto, authentication));
    }

    /* ====================================
       UPDATE
       ==================================== */

    @TenantManagerOnly
    @PutMapping("/{id}")
    public ResponseEntity<DepartmentDTO> updateDepartment(
            @PathVariable UUID id,
            @RequestBody DepartmentDTO dto,
            Authentication authentication
    ) {

        DepartmentDTO updated =
                departmentService.updateDepartment(id, dto, authentication);

        return ResponseEntity.ok(updated);
    }

    /* ====================================
       READ
       ==================================== */

    @TenantSupervisorOnly
    @GetMapping("/{id}")
    public ResponseEntity<DepartmentDTO> get(
            @PathVariable UUID id
    ) {

        return ResponseEntity.ok(
                departmentMapper.toDTO(
                        departmentService.getById(id)
                )
        );
    }

    @TenantSupervisorOnly
    @GetMapping
    public ResponseEntity<List<DepartmentDTO>> getAllDepartments(
            @RequestParam(required = false) Boolean deleted
    ) {

        return ResponseEntity.ok(
                departmentService.getAllDepartments(deleted)
        );
    }

    @TenantManagerOnly
    @GetMapping("/user/{userId}")
    public ResponseEntity<List<DepartmentMinimalDTO>> getUserDepartments(
            @PathVariable UUID userId
    ) {

        return ResponseEntity.ok(
                departmentService.getAllDepartmentsForUser(userId)
        );
    }

    /* ====================================
       AUDITS
       ==================================== */

    @TenantManagerOnly
    @GetMapping("/{id}/audits")
    public ResponseEntity<List<DepartmentAudit>> getDepartmentAudits(
            @PathVariable UUID id
    ) {

        return ResponseEntity.ok(
                departmentService.getDepartmentAudits(id)
        );
    }

    @TenantManagerOnly
    @GetMapping("/all/audits")
    public ResponseEntity<List<DepartmentAudit>> getAllDepartmentsAudits() {

        return ResponseEntity.ok(
                departmentService.getAllDepartmentsAudits()
        );
    }

    @TenantManagerOnly
    @GetMapping("/audits/performer/{id}")
    public ResponseEntity<List<DepartmentAudit>> getDepartmentAuditsByPerformer(
            @PathVariable UUID id
    ) {

        return ResponseEntity.ok(
                departmentService.getDepartmentAuditsByPerformer(id)
        );
    }

    /* ====================================
       DELETE
       ==================================== */

    @TenantManagerOnly
    @DeleteMapping("/delete/{id}")
    public ResponseEntity<ApiResponse> deleteDepartment(
            @PathVariable UUID id,
            @RequestParam(defaultValue = "true") Boolean soft,
            Authentication authentication
    ) {

        departmentService.deleteDepartment(id, soft, authentication);

        ApiResponse response =
                new ApiResponse(
                        "success",
                        "Department " + (Boolean.TRUE.equals(soft) ? "soft" : "hard") + " deleted successfully"
                );

        return ResponseEntity.ok(response);
    }

    @TenantManagerOnly
    @DeleteMapping("/delete/bulk")
    public ResponseEntity<ApiResponse> deleteDepartmentsInBulk(
            @RequestBody List<UUID> ids,
            @RequestParam(defaultValue = "true") Boolean soft,
            Authentication authentication
    ) {

        for (UUID id : ids) {
            departmentService.deleteDepartment(id, soft, authentication);
        }

        ApiResponse response =
                new ApiResponse(
                        "success",
                        "Departments " + (Boolean.TRUE.equals(soft) ? "soft" : "hard") + " deleted successfully"
                );

        return ResponseEntity.ok(response);
    }

    /* ====================================
       RESTORE
       ==================================== */

    @TenantManagerOnly
    @PatchMapping("/restore/{id}")
    public ResponseEntity<ApiResponse> restoreDepartment(
            @PathVariable UUID id,
            Authentication authentication
    ) {

        departmentService.restoreDepartment(id, authentication);

        ApiResponse response =
                new ApiResponse(
                        "success",
                        "Department restored successfully"
                );

        return ResponseEntity.ok(response);
    }

    @TenantManagerOnly
    @PatchMapping("/restore/bulk")
    public ResponseEntity<ApiResponse> restoreDepartmentsInBulk(
            @RequestBody List<UUID> ids,
            Authentication authentication
    ) {

        for (UUID id : ids) {
            departmentService.restoreDepartment(id, authentication);
        }

        ApiResponse response =
                new ApiResponse(
                        "success",
                        "Departments restored successfully"
                );

        return ResponseEntity.ok(response);
    }
}