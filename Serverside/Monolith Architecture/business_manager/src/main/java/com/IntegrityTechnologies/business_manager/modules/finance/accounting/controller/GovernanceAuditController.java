package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.GovernanceAuditLog;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.GovernanceAuditLogRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/accounting/governance")
@RequiredArgsConstructor
public class GovernanceAuditController {

    private final GovernanceAuditLogRepository repository;

    @GetMapping
    public Page<GovernanceAuditLog> logs(
            @RequestParam UUID branchId,
            @PageableDefault(size = 100, sort = "performedAt") Pageable pageable
    ) {
        SecurityUtils.requireAtLeast(Role.ADMIN);
        return repository.findByBranchId(branchId, pageable);
    }
}