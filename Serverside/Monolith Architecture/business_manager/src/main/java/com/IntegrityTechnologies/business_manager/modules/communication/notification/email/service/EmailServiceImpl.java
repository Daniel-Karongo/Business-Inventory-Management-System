package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.dto.EmailRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model.BranchEmailSettings;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model.EmailMessage;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.repository.BranchEmailSettingsRepository;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.repository.EmailMessageRepository;
import com.IntegrityTechnologies.business_manager.security.util.BranchTenantGuard;
import com.IntegrityTechnologies.business_manager.security.util.TenantAwareExecutor;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.mail.internet.MimeMessage;
import jakarta.persistence.EntityNotFoundException;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.mail.javamail.JavaMailSenderImpl;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.Properties;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class EmailServiceImpl implements EmailService {

    private static final Logger log =
            LoggerFactory.getLogger(EmailServiceImpl.class);

    private final EmailMessageRepository repo;
    private final BranchEmailSettingsRepository settingsRepo;
    private final BranchTenantGuard branchTenantGuard;

    private UUID tenantId() {
        return TenantContext.getTenantId();
    }

    @Override
    @Transactional
    public EmailMessage send(
            UUID branchId,
            EmailRequest request
    ) {

        branchTenantGuard.validate(branchId);

        BranchEmailSettings settings =
                settingsRepo
                        .findByTenantIdAndBranchIdAndActiveTrueAndDeletedFalse(
                                tenantId(),
                                branchId
                        )
                        .orElseThrow(() ->
                                new IllegalStateException(
                                        "Email not configured for branch"
                                )
                        );

        if (!Boolean.TRUE.equals(settings.getEnabled())) {
            throw new IllegalStateException(
                    "Email disabled for branch"
            );
        }

        EmailMessage msg =
                EmailMessage.builder()
                        .tenantId(tenantId())
                        .branchId(branchId)
                        .recipients(request.getTo())
                        .subject(request.getSubject())
                        .body(request.getBody())
                        .status("QUEUED")
                        .createdBy(request.getCreatedBy())
                        .build();

        repo.save(msg);

        repo.flush();

        sendAsync(
                tenantId(),
                msg.getId(),
                branchId
        );

        return msg;
    }

    @Override
    public EmailMessage get(
            UUID branchId,
            UUID messageId
    ) {

        branchTenantGuard.validate(branchId);

        return repo
                .findByTenantIdAndBranchIdAndId(
                        tenantId(),
                        branchId,
                        messageId
                )
                .orElseThrow(() ->
                        new EntityNotFoundException(
                                "Email message not found"
                        )
                );
    }

    @Async("tenantAwareExecutor")
    @Transactional
    public void sendAsync(
            UUID tenantId,
            UUID messageId,
            UUID branchId
    ) {

        TenantContext.setTenantId(tenantId);

        try {

            EmailMessage msg =
                    repo.findByTenantIdAndBranchIdAndId(
                                    tenantId(),
                                    branchId,
                                    messageId
                            )
                            .orElseThrow();

            BranchEmailSettings settings =
                    settingsRepo
                            .findByTenantIdAndBranchIdAndActiveTrueAndDeletedFalse(
                                    tenantId(),
                                    branchId
                            )
                            .orElseThrow();

            JavaMailSenderImpl sender =
                    buildSender(settings);

            MimeMessage mime =
                    sender.createMimeMessage();

            MimeMessageHelper helper =
                    new MimeMessageHelper(
                            mime,
                            true,
                            "UTF-8"
                    );

            helper.setFrom(settings.getFromAddress());

            helper.setTo(
                    msg.getRecipients().toArray(new String[0])
            );

            helper.setSubject(msg.getSubject());

            helper.setText(msg.getBody(), true);

            sender.send(mime);

            msg.setStatus("SENT");
            msg.setSentAt(LocalDateTime.now());

            repo.save(msg);

        } catch (Exception e) {

            log.error("Email send failed", e);

            try {

                EmailMessage failed =
                        repo.findByTenantIdAndBranchIdAndId(
                                        tenantId(),
                                        branchId,
                                        messageId
                                )
                                .orElse(null);

                if (failed != null) {

                    failed.setStatus("FAILED");
                    failed.setError(e.getMessage());

                    failed.setNextRetryAt(
                            LocalDateTime.now()
                                    .plusMinutes(
                                            Math.max(
                                                    1,
                                                    failed.getRetryCount()
                                            ) * 5L
                                    )
                    );

                    repo.save(failed);
                }

            } catch (Exception ignored) {
            }
        } finally {
            TenantContext.clear();
        }
    }

    private JavaMailSenderImpl buildSender(
            BranchEmailSettings settings
    ) {

        JavaMailSenderImpl sender =
                new JavaMailSenderImpl();

        sender.setHost(settings.getHost());
        sender.setPort(settings.getPort());
        sender.setUsername(settings.getUsername());
        sender.setPassword(settings.getPassword());

        Properties props =
                sender.getJavaMailProperties();

        props.put(
                "mail.smtp.auth",
                settings.getAuthEnabled()
        );

        props.put(
                "mail.smtp.starttls.enable",
                settings.getTlsEnabled()
        );

        return sender;
    }
}