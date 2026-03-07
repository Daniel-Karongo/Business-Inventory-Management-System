package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.GovernanceAuditLogResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.GovernanceAuditLogRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantAdminOnly;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/accounting/governance")
@RequiredArgsConstructor
@TenantAdminOnly
public class GovernanceAuditController {

    private final GovernanceAuditLogRepository repository;

    @GetMapping
    public Page<GovernanceAuditLogResponse> logs(
            @RequestParam UUID branchId,
            @PageableDefault(size = 100, sort = "performedAt") Pageable pageable
    ) {
        return repository.findByBranchId(branchId, pageable)
                .map(GovernanceAuditLogResponse::from);
    }
}