package com.IntegrityTechnologies.business_manager.modules.platform.audit.service;

import com.IntegrityTechnologies.business_manager.modules.platform.audit.entity.AuditEntityType;
import com.IntegrityTechnologies.business_manager.modules.platform.audit.entity.AuditLog;
import com.IntegrityTechnologies.business_manager.modules.platform.audit.repository.AuditRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class AuditService {
    private final AuditRepository auditRepository;
    public void log(
            AuditEntityType entityType,
            UUID entityId,
            String action,
            String before,
            String after
    ) {

        UUID tenantId = TenantContext.getTenantIdOrNull();

        auditRepository.save(

                AuditLog.builder()
                        .tenantId(tenantId)
                        .userId(SecurityUtils.currentUserId())
                        .entityType(entityType)
                        .entityId(entityId)
                        .action(action)
                        .beforeState(before)
                        .afterState(after)
                        .timestamp(LocalDateTime.now())
                        .build()

        );

    }
}