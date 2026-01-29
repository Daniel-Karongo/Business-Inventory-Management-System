package com.IntegrityTechnologies.business_manager.security.acl.controller;

import com.IntegrityTechnologies.business_manager.security.acl.entity.RolePermission;
import com.IntegrityTechnologies.business_manager.security.acl.repository.RolePermissionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/admin/acl/role-permissions")
@RequiredArgsConstructor
@PreAuthorize("hasRole('SUPERUSER')")
public class RolePermissionAdminController {

    private final RolePermissionRepository repo;

    @GetMapping
    public List<RolePermission> list() {
        return repo.findAll();
    }

    @PostMapping
    public RolePermission assign(@RequestBody RolePermission rp) {
        return repo.save(rp);
    }

    @DeleteMapping("/{id}")
    public void revoke(@PathVariable UUID id) {
        repo.deleteById(id);
    }
}