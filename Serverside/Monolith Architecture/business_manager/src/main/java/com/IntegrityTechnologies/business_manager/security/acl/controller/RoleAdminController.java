package com.IntegrityTechnologies.business_manager.security.acl.controller;

import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.security.acl.audit.AclAuditService;
import com.IntegrityTechnologies.business_manager.security.acl.entity.RoleEntity;
import com.IntegrityTechnologies.business_manager.security.acl.repository.PermissionConditionRepository;
import com.IntegrityTechnologies.business_manager.security.acl.repository.RoleEntityRepository;
import com.IntegrityTechnologies.business_manager.security.acl.repository.RolePermissionRepository;
import com.IntegrityTechnologies.business_manager.security.acl.service.PermissionCacheService;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/admin/acl/roles")
@RequiredArgsConstructor
@PreAuthorize("hasRole('SUPERUSER')")
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
                id.toString() +
                        " (rolePermissions=" + permsDisabled +
                        ", conditions=" + condsDisabled + ")"
        );
    }
}