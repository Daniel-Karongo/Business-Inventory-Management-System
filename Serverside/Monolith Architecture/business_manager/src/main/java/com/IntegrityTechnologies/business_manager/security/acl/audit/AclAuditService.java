package com.IntegrityTechnologies.business_manager.security.acl.audit;

import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.web.context.request.*;

import java.time.Instant;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class AclAuditService {

    private final AclAuditLogRepository repo;
    private final ObjectMapper mapper = new ObjectMapper();

    public void audit(
            String entityType,
            String action,
            Object before,
            Object after,
            String entityId
    ) {
        repo.save(
                AclAuditLog.builder()
                        .entityType(entityType)
                        .action(action)
                        .entityId(entityId)
                        .beforeState(toJson(before))
                        .afterState(toJson(after))
                        .actorUserId(SecurityUtils.currentUserId())
                        .actorUsername(SecurityUtils.currentUsername())
                        .actorRole(SecurityUtils.currentRole().name())
                        .sourceIp(resolveIp())
                        .createdAt(Instant.now())
                        .build()
        );
    }

    private String toJson(Object o) {
        try {
            return o == null ? null : mapper.writeValueAsString(o);
        } catch (Exception e) {
            return "{\"error\":\"serialization_failed\"}";
        }
    }

    private String resolveIp() {
        ServletRequestAttributes a =
                (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
        HttpServletRequest r = a != null ? a.getRequest() : null;
        return r != null ? r.getRemoteAddr() : null;
    }
}