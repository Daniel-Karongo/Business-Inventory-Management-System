package com.IntegrityTechnologies.business_manager.modules.platform.audit.controller;

import com.IntegrityTechnologies.business_manager.modules.platform.audit.entity.AuditLog;
import com.IntegrityTechnologies.business_manager.modules.platform.audit.repository.AuditRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/platform/audit")
@RequiredArgsConstructor
public class AuditController {

    private final AuditRepository repository;

    @GetMapping
    public Page<AuditLog> getAudit(Pageable pageable) {

        return repository.findAll(pageable);

    }

}