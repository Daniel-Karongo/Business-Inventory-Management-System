package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model.EmailMessage;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.repository.EmailMessageRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.service.TenantExecutionService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class EmailRetryScheduler {

    private final EmailMessageRepository repo;
    private final EmailServiceImpl emailService;
    private final TenantExecutionService tenantExecutionService;

    @Scheduled(fixedDelay = 300_000)
    public void retryFailedEmails() {

        tenantExecutionService.forEachTenant(tenantId -> {

            List<EmailMessage> failed =
                    repo.findByStatus("FAILED");

            for (EmailMessage msg : failed) {
                log.info("Retrying failed email {}", msg.getId());
                emailService.sendAsync(msg);
            }
        });
    }
}