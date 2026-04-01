package com.IntegrityTechnologies.business_manager.security.audit.service;

import com.IntegrityTechnologies.business_manager.security.audit.model.LoginAudit;
import com.IntegrityTechnologies.business_manager.security.audit.repository.LoginAuditRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class LoginAuditService {

    private final LoginAuditRepository repo;

    public void log(
            UUID tenantId,
            UUID userId,
            UUID branchId,
            String fingerprint,
            Double lat,
            Double lng,
            Double accuracy,
            String ip,
            String status,
            String reason
    ) {
        repo.save(
                LoginAudit.builder()
                        .tenantId(tenantId)
                        .userId(userId)
                        .branchId(branchId)
                        .fingerprint(fingerprint)
                        .latitude(lat)
                        .longitude(lng)
                        .accuracy(accuracy)
                        .ip(ip)
                        .status(status)
                        .reason(reason)
                        .build()
        );
    }
}