package com.IntegrityTechnologies.business_manager.modules.platform.audit.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.audit.entity.AuditLog;
import com.IntegrityTechnologies.business_manager.modules.platform.audit.repository.AuditRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.PlatformAdminOnly;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Platform Audit")
@RestController
@RequestMapping("/api/platform/audit")
@RequiredArgsConstructor
@PlatformAdminOnly
public class AuditController {

    private final AuditRepository repository;

    @GetMapping
    public Page<AuditLog> getAudit(
            Pageable pageable
    ) {

        return repository.findAll(pageable);

    }

}