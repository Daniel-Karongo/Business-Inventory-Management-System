package com.IntegrityTechnologies.business_manager.security.acl.controller;

import com.IntegrityTechnologies.business_manager.security.acl.entity.Permission;
import com.IntegrityTechnologies.business_manager.security.acl.repository.PermissionRepository;
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

    private final PermissionRepository repo;

    @GetMapping
    public List<Permission> list() {
        return repo.findAll();
    }

    @PostMapping
    public Permission create(@RequestBody Permission p) {
        return repo.save(p);
    }

    @PutMapping("/{id}")
    public Permission update(@PathVariable UUID id, @RequestBody Permission p) {
        p.setId(id);
        return repo.save(p);
    }

    @DeleteMapping("/{id}")
    public void delete(@PathVariable UUID id) {
        repo.deleteById(id);
    }
}