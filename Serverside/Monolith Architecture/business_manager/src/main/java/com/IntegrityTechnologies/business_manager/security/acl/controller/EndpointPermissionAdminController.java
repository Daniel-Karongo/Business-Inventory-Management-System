package com.IntegrityTechnologies.business_manager.security.acl.controller;

import com.IntegrityTechnologies.business_manager.security.acl.entity.EndpointPermission;
import com.IntegrityTechnologies.business_manager.security.acl.repository.EndpointPermissionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/admin/acl/endpoints")
@RequiredArgsConstructor
@PreAuthorize("hasRole('SUPERUSER')")
public class EndpointPermissionAdminController {

    private final EndpointPermissionRepository repo;

    @GetMapping
    public List<EndpointPermission> list() {
        return repo.findAll();
    }

    @PostMapping
    public EndpointPermission assign(@RequestBody EndpointPermission ep) {
        return repo.save(ep);
    }

    @DeleteMapping("/{id}")
    public void delete(@PathVariable Long id) {
        repo.deleteById(id);
    }
}