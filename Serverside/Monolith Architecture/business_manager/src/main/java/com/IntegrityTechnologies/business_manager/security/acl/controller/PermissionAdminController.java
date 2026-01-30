package com.IntegrityTechnologies.business_manager.security.acl.controller;

import com.IntegrityTechnologies.business_manager.security.acl.audit.AclAuditService;
import com.IntegrityTechnologies.business_manager.security.acl.entity.Permission;
import com.IntegrityTechnologies.business_manager.security.acl.repository.EndpointPermissionRepository;
import com.IntegrityTechnologies.business_manager.security.acl.repository.PermissionConditionRepository;
import com.IntegrityTechnologies.business_manager.security.acl.repository.PermissionRepository;
import com.IntegrityTechnologies.business_manager.security.acl.repository.RolePermissionRepository;
import com.IntegrityTechnologies.business_manager.security.acl.service.PermissionCacheService;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/admin/acl/permissions")
@RequiredArgsConstructor
@PreAuthorize("hasRole('SUPERUSER')")
public class PermissionAdminController {

    private final AclAuditService audit;
    private final PermissionRepository permissionRepo;
    private final EndpointPermissionRepository endpointRepo;
    private final RolePermissionRepository rolePermRepo;
    private final PermissionConditionRepository conditionRepo;
    private final PermissionCacheService cache;

    @GetMapping
    public List<Permission> list() {
        return permissionRepo.findAll();
    }

    @PostMapping
    public Permission create(@RequestBody Permission p) {

        Permission saved = permissionRepo.save(p);

        audit.audit(
                "PERMISSION",
                "CREATE",
                null,
                saved,
                saved.getId().toString()
        );

        return saved;
    }

    @PutMapping("/{id}")
    public Permission update(@PathVariable UUID id, @RequestBody Permission p) {

        Permission before = permissionRepo.findById(id).orElseThrow();
        p.setId(id);
        Permission saved = permissionRepo.save(p);

        audit.audit(
                "PERMISSION",
                "UPDATE",
                before,
                saved,
                id.toString()
        );

        return saved;
    }

    @DeleteMapping("/{id}")
    public void softDeletePermission(@PathVariable UUID id) {

        Permission permission = permissionRepo.findById(id).orElseThrow();

        permissionRepo.softDelete(id);
        endpointRepo.softDeleteByPermissionId(id);
        rolePermRepo.findAll().stream()
                .filter(rp -> rp.getPermission().getId().equals(id))
                .forEach(rp -> rp.setActive(false));
        conditionRepo.softDeleteByPermissionId(id);

        cache.refresh();

        audit.audit(
                "PERMISSION",
                "DEACTIVATE",
                permission,
                null,
                id.toString()
        );
    }
}