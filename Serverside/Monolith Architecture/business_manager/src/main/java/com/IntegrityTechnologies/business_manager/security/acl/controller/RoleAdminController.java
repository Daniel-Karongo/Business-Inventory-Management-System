package com.IntegrityTechnologies.business_manager.security.acl.controller;

import com.IntegrityTechnologies.business_manager.security.acl.entity.RoleEntity;
import com.IntegrityTechnologies.business_manager.security.acl.repository.RoleEntityRepository;
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

    @GetMapping
    public List<RoleEntity> list() {
        return repo.findAll();
    }

    @PostMapping
    public RoleEntity create(@RequestBody RoleEntity r) {
        return repo.save(r);
    }

    @PutMapping("/{id}")
    public RoleEntity update(@PathVariable UUID id, @RequestBody RoleEntity r) {
        r.setId(id);
        return repo.save(r);
    }

    @DeleteMapping("/{id}")
    public void delete(@PathVariable UUID id) {
        repo.deleteById(id);
    }
}