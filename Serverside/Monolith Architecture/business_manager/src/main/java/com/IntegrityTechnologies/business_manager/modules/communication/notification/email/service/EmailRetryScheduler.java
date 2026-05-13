package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.base.BranchNotificationSettings;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.base.BranchNotificationSettingsRepository;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model.EmailMessage;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.repository.EmailMessageRepository;
import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.service.TenantExecutionService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Slf4j
@Component
@RequiredArgsConstructor
public class EmailRetryScheduler {

    private final EmailMessageRepository repo;
    private final EmailServiceImpl emailService;
    private final TenantExecutionService tenantExecutionService;
    private final BranchRepository branchRepository;
    private final BranchNotificationSettingsRepository notificationRepo;

    @Scheduled(fixedDelay = 300_000)
    public void retryFailedEmails() {

        tenantExecutionService.forEachTenant(tenantId -> {

            List<UUID> branchIds =
                    branchRepository
                            .findByTenantIdAndDeletedFalse(tenantId)
                            .stream()
                            .map(b -> b.getId())
                            .toList();

            for (UUID branchId : branchIds) {

                BranchNotificationSettings settings =
                        notificationRepo
                                .findByTenantIdAndBranchIdAndDeletedFalse(
                                        tenantId,
                                        branchId
                                )
                                .orElse(null);

                if (settings == null
                        || !Boolean.TRUE.equals(settings.getAllowRetries())
                        || !Boolean.TRUE.equals(settings.getEmailEnabled())) {
                    continue;
                }

                List<EmailMessage> retryable =
                        repo.findRetryableMessages(
                                tenantId,
                                branchId,
                                List.of(
                                        "FAILED",
                                        "QUEUED"
                                ),
                                settings.getMaxRetryCount(),
                                LocalDateTime.now()
                        );

                for (EmailMessage msg : retryable) {

                    try {

                        log.info(
                                "Retrying failed email {}",
                                msg.getId()
                        );

                        msg.setRetryCount(
                                msg.getRetryCount() + 1
                        );

                        repo.save(msg);

                        emailService.sendAsync(
                                tenantId,
                                msg.getId(),
                                branchId
                        );

                    } catch (Exception ex) {

                        log.error(
                                "Email retry failed",
                                ex
                        );
                    }
                }
            }
        });
    }
}