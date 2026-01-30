package com.IntegrityTechnologies.business_manager.modules.person.entity.department.controller;

import com.IntegrityTechnologies.business_manager.common.ApiResponse;
import com.IntegrityTechnologies.business_manager.common.PrivilegesChecker;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.mapper.DepartmentMapper;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.service.DepartmentService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto.DepartmentDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.dto.DepartmentMinimalDTO;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.DepartmentAudit;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/departments")
@RequiredArgsConstructor
@Tag(name = "Departments")
public class DepartmentController {

    private final DepartmentService departmentService;
    private final DepartmentMapper departmentMapper;

    @PostMapping
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    public ResponseEntity<DepartmentDTO> create(
            @RequestBody DepartmentDTO dto,
            Authentication authentication
    ) {
        return ResponseEntity.status(HttpStatus.CREATED).body(departmentService.create(dto, authentication));
    }

    @PostMapping("/bulk")
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    public ResponseEntity<List<DepartmentDTO>> createDepartmentsInBulk(
            @RequestBody List<DepartmentDTO> dtos,
            Authentication authentication
    ) {
        List<DepartmentDTO> createdDTOs = new ArrayList<>();
        for(DepartmentDTO dto: dtos) {
            createdDTOs.add(departmentService.create(dto, authentication));
        }
        return ResponseEntity.status(HttpStatus.CREATED).body(createdDTOs);
    }

    @PutMapping("/{id}")
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    public ResponseEntity<DepartmentDTO> updateDepartment(
            @PathVariable UUID id,
            @RequestBody DepartmentDTO dto,
            Authentication authentication
    ) {
        DepartmentDTO updatedDto = departmentService.updateDepartment(id, dto, authentication);
        return ResponseEntity.ok(updatedDto);
    }





    @GetMapping("/{id}")
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER','SUPERVISOR')")
    public ResponseEntity<DepartmentDTO> get(@PathVariable UUID id) {
        return ResponseEntity.ok(departmentMapper.toDTO(departmentService.getById(id)));
    }

    // --- Get all departments ---
    @GetMapping
    @PreAuthorize("""
    (#deleted == true or #deleted == null) and hasRole('SUPERUSER') 
    or 
    (#deleted == false and hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER','SUPERVISOR'))""")
    public ResponseEntity<List<DepartmentDTO>> getAllDepartments(
            @RequestParam(required = false) Boolean deleted
    ) {
        return ResponseEntity.ok(departmentService.getAllDepartments(deleted));
    }

    @GetMapping("/user/{userId}")
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    public ResponseEntity<List<DepartmentMinimalDTO>> getUserDepartments(
            @PathVariable UUID userId
    ) {
        return ResponseEntity.ok(departmentService.getAllDepartmentsForUser(userId));
    }

    @GetMapping("/{id}/audits")
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    public ResponseEntity<List<DepartmentAudit>> getDepartmentAudits(@PathVariable UUID id) {
        return ResponseEntity.ok(departmentService.getDepartmentAudits(id));
    }

    @GetMapping("/all/audits")
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    public ResponseEntity<List<DepartmentAudit>> getAllDepartmentsAudits() {
        return ResponseEntity.ok(departmentService.getAllDepartmentsAudits());
    }

    @GetMapping("/audits/performer/{id}")
    @PreAuthorize("hasAnyRole('SUPERUSER','ADMIN','MANAGER')")
    public ResponseEntity<List<DepartmentAudit>> getDepartmentAuditsByPerformer(@PathVariable UUID id) {
        return ResponseEntity.ok(departmentService.getDepartmentAuditsByPerformer(id));
    }




    // --- Delete a department ---
    @DeleteMapping("/delete/{id}")
    @PreAuthorize("""
    #soft == false and hasRole('SUPERUSER')
    or 
    ( #soft == true and hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER'))""")
    public ResponseEntity<?> deleteDepartment(
            @PathVariable UUID id,
            @RequestParam(defaultValue = "true") Boolean soft,
            Authentication authentication
    ) {
        departmentService.deleteDepartment(id, soft, authentication);
        ApiResponse response = new ApiResponse(
                "success",
                "Department" + (Boolean.TRUE.equals(soft) ? "soft" : "hard") + " deleted successfully"
        );
        return ResponseEntity.ok(response);
    }
    @DeleteMapping("/delete/bulk")
    @PreAuthorize("""
    #soft == false and hasRole('SUPERUSER')
    or 
    ( #soft == true and hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER'))""")
    public ResponseEntity<?> deleteDepartmentsInBulk (
            @RequestBody List<UUID> ids,
            @RequestParam(defaultValue = "true") Boolean soft,
            Authentication authentication
    ) {
        for(UUID id: ids) {
            departmentService.deleteDepartment(id, soft, authentication);
        }
        ApiResponse response = new ApiResponse(
                "success",
                "Departments" + (Boolean.TRUE.equals(soft) ? "soft" : "hard") + " deleted successfully"
        );
        return ResponseEntity.ok(response);
    }

    @PatchMapping("/restore/{id}")
    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    public ResponseEntity<?> restoreDepartment(
            @PathVariable UUID id,
            Authentication authentication
    ) {
        departmentService.restoreDepartment(id, authentication);
        ApiResponse response = new ApiResponse(
                "success",
                "Department restored successfully"
        );
        return ResponseEntity.ok(response);
    }

    @PatchMapping("/restore/bulk")
    @PreAuthorize("hasAnyRole('SUPERUSER', 'ADMIN', 'MANAGER')")
    public ResponseEntity<?> restoreDepartmentsInBulk(
            @RequestBody List<UUID> ids,
            Authentication authentication
    ) {
        for(UUID id: ids) {
            departmentService.restoreDepartment(id, authentication);
        }
        ApiResponse response = new ApiResponse(
                "success",
                "Departments restored successfully"
        );
        return ResponseEntity.ok(response);
    }
}