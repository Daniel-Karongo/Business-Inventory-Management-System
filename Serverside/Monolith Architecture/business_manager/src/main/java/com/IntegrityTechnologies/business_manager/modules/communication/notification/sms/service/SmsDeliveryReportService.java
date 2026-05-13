package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsMessage;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.repository.SmsMessageRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class SmsDeliveryReportService {

    private final SmsMessageRepository repo;
    private final BranchTenantGuard branchTenantGuard;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Transactional
    public void handleDeliveryReport(
            UUID branchId,
            Map<String, String> payload
    ) {

        branchTenantGuard.validate(branchId);

        String messageId = payload.get("id");
        String status = payload.get("status");

        if (messageId == null) {
            return;
        }

        SmsMessage msg =
                repo.findByTenantIdAndBranchIdAndProviderMessageId(
                                tenantId(),
                                branchId,
                                messageId
                        )
                        .orElse(null);

        if (msg == null) {
            return;
        }

        if ("Success".equalsIgnoreCase(status)) {
            msg.setStatus("DELIVERED");
        } else {
            msg.setStatus("FAILED");
            msg.setError(payload.toString());
        }

        repo.save(msg);
    }
}