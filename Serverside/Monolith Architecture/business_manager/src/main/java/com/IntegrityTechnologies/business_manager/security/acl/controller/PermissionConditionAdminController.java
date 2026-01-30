package com.IntegrityTechnologies.business_manager.security.acl.controller;

import com.IntegrityTechnologies.business_manager.security.acl.audit.AclAuditService;
import com.IntegrityTechnologies.business_manager.security.acl.entity.Permission;
import com.IntegrityTechnologies.business_manager.security.acl.entity.PermissionCondition;
import com.IntegrityTechnologies.business_manager.security.acl.entity.RoleEntity;
import com.IntegrityTechnologies.business_manager.security.acl.repository.PermissionConditionRepository;
import com.IntegrityTechnologies.business_manager.security.acl.repository.PermissionRepository;
import com.IntegrityTechnologies.business_manager.security.acl.repository.RoleEntityRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;
import java.util.UUID;

@RestController
@RequestMapping("/api/admin/acl")
@RequiredArgsConstructor
@PreAuthorize("hasRole('SUPERUSER')")
public class PermissionConditionAdminController {

    private final PermissionConditionRepository conditionRepo;
    private final PermissionRepository permissionRepo;
    private final RoleEntityRepository roleRepo;
    private final AclAuditService audit;

    /* =====================================================
       LIST CONDITIONS (permission + role)
       ===================================================== */
    @GetMapping("/permissions/{permissionId}/conditions")
    public List<PermissionCondition> list(
            @PathVariable UUID permissionId,
            @RequestParam String role
    ) {
        return conditionRepo
                .findByPermission_IdAndRole_NameIgnoreCaseAndActiveTrue(
                        permissionId,
                        role
                );
    }

    /* =====================================================
       CREATE CONDITION (OR row)
       ===================================================== */
    @PostMapping("/permissions/{permissionId}/conditions")
    public PermissionCondition create(
            @PathVariable UUID permissionId,
            @RequestBody CreateConditionRequest req
    ) {
        Permission permission =
                permissionRepo.findById(permissionId).orElseThrow();

        RoleEntity role =
                roleRepo.findByNameIgnoreCase(req.role())
                        .orElseThrow();

        PermissionCondition saved =
                conditionRepo.save(
                        PermissionCondition.builder()
                                .permission(permission)
                                .role(role)
                                .param(req.param())
                                .operator("EQ") // locked for now
                                .value(req.value())
                                .active(true)
                                .build()
                );

        audit.audit(
                "CONDITION",
                "CREATE",
                null,
                saved,
                saved.getId().toString()
        );

        return saved;
    }

    /* =====================================================
       SOFT DELETE CONDITION
       ===================================================== */
    @DeleteMapping("/conditions/{conditionId}")
    public void softDelete(
            @PathVariable UUID conditionId,
            @RequestBody(required = false) Map<String, String> body
    ) {
        PermissionCondition before =
                conditionRepo.findById(conditionId).orElseThrow();

        before.setActive(false);
        conditionRepo.save(before);

        audit.audit(
                "CONDITION",
                "DEACTIVATE",
                before,
                null,
                conditionId.toString()
        );
    }

    /* =====================================================
       REQUEST DTO
       ===================================================== */
    public record CreateConditionRequest(
            String role,
            String param,
            String value
    ) {}
}