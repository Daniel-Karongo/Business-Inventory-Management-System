package com.IntegrityTechnologies.business_manager.modules.finance.accounting.controller;

import com.IntegrityTechnologies.business_manager.modules.finance.accounting.dto.GovernanceAuditLogResponse;
import com.IntegrityTechnologies.business_manager.modules.finance.accounting.governance.GovernanceAuditLogRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.TenantAdminOnly;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.IntegrityTechnologies.business_manager.security.BranchTenantGuard;
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
    private final BranchTenantGuard branchTenantGuard;

    @GetMapping
    public Page<GovernanceAuditLogResponse> logs(
            @RequestParam UUID branchId,
            @PageableDefault(size = 100, sort = "performedAt") Pageable pageable
    ) {
        UUID tenantId = TenantContext.getTenantId();
        branchTenantGuard.validate(branchId);
        return repository.findByTenantIdAndBranchId(tenantId, branchId, pageable)
                .map(GovernanceAuditLogResponse::from);
    }
}