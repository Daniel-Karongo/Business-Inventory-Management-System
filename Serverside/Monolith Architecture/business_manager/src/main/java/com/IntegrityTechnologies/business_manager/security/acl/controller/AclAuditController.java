package com.IntegrityTechnologies.business_manager.security.acl.controller;

import com.IntegrityTechnologies.business_manager.security.acl.audit.AclAuditLog;
import com.IntegrityTechnologies.business_manager.security.acl.audit.AclAuditLogRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/admin/acl/audits")
@RequiredArgsConstructor
@PreAuthorize("hasRole('SUPERUSER')")
public class AclAuditController {

    private final AclAuditLogRepository repo;

    /**
     * Recent ACL activity (most recent first)
     */
    @GetMapping
    public List<AclAuditLog> recent(
            @RequestParam(defaultValue = "50") int limit
    ) {
        return repo.findAll(
                PageRequest.of(
                        0,
                        limit,
                        Sort.by(Sort.Direction.DESC, "createdAt")
                )
        ).getContent();
    }

    /**
     * Full audit trail for a specific entity
     */
    @GetMapping("/{entityType}/{entityId}")
    public List<AclAuditLog> byEntity(
            @PathVariable String entityType,
            @PathVariable String entityId
    ) {
        return repo.findByEntityTypeAndEntityIdOrderByCreatedAtDesc(
                entityType,
                entityId
        );
    }
}