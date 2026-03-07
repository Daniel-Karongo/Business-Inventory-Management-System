package com.IntegrityTechnologies.business_manager.modules.acl.controller;

import com.IntegrityTechnologies.business_manager.modules.acl.audit.AclAuditService;
import com.IntegrityTechnologies.business_manager.modules.acl.entity.RoleEntity;
import com.IntegrityTechnologies.business_manager.modules.acl.repository.*;
import com.IntegrityTechnologies.business_manager.modules.acl.service.PermissionCacheService;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformAdminOnly;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/admin/acl/roles")
@RequiredArgsConstructor
@PlatformAdminOnly
public class RoleAdminController {

    private final RoleEntityRepository repo;
    private final AclAuditService audit;
    private final RoleEntityRepository roleRepo;
    private final RolePermissionRepository rolePermRepo;
    private final PermissionConditionRepository conditionRepo;
    private final PermissionCacheService cache;

    @GetMapping
    public List<RoleEntity> list() {
        return repo.findAll();
    }

    @PostMapping
    public RoleEntity create(@RequestBody RoleEntity r) {

        RoleEntity saved = repo.save(r);

        audit.audit(
                "ROLE",
                "CREATE",
                null,
                saved,
                saved.getId().toString()
        );

        return saved;
    }

    @PutMapping("/{id}")
    public RoleEntity update(@PathVariable UUID id, @RequestBody RoleEntity r) {

        RoleEntity before = repo.findById(id).orElseThrow();
        r.setId(id);

        RoleEntity saved = repo.save(r);

        audit.audit(
                "ROLE",
                "UPDATE",
                before,
                saved,
                id.toString()
        );

        return saved;
    }

    @DeleteMapping("/{id}")
    public void softDeleteRole(@PathVariable UUID id) {

        RoleEntity role = roleRepo.findById(id).orElseThrow();

        if (!role.isActive()) {
            throw new IllegalStateException("Role already inactive");
        }

        role.setActive(false);
        roleRepo.save(role);

        int permsDisabled = rolePermRepo.softDeleteByRoleId(id);
        int condsDisabled = conditionRepo.softDeleteByRoleId(id);

        cache.refresh();

        audit.audit(
                "ROLE",
                "DEACTIVATE",
                role,
                null,
                id.toString()
                        + " (rolePermissions=" + permsDisabled
                        + ", conditions=" + condsDisabled + ")"
        );
    }
}