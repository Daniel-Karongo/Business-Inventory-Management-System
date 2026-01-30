package com.IntegrityTechnologies.business_manager.security.acl.controller;

import com.IntegrityTechnologies.business_manager.security.acl.audit.AclAuditService;
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
    private final AclAuditService audit;

    @GetMapping
    public List<EndpointPermission> list() {
        return repo.findAll();
    }

    @PostMapping
    public EndpointPermission assign(@RequestBody EndpointPermission ep) {

        EndpointPermission saved = repo.save(ep);

        audit.audit(
                "ENDPOINT_PERMISSION",
                "CREATE",
                null,
                saved,
                saved.getId().toString()
        );

        return saved;
    }

    @DeleteMapping("/{id}")
    public void delete(@PathVariable Long id) {

        EndpointPermission before = repo.findById(id).orElseThrow();
        repo.deleteById(id);

        audit.audit(
                "ENDPOINT_PERMISSION",
                "DELETE",
                before,
                null,
                id.toString()
        );
    }
}