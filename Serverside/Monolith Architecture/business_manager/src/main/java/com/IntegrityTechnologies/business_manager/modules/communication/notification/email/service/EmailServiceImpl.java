package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.dto.EmailRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model.EmailMessage;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.repository.EmailMessageRepository;
import jakarta.mail.internet.MimeMessage;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;

@Service
@RequiredArgsConstructor
public class EmailServiceImpl implements EmailService {

    private static final Logger log = LoggerFactory.getLogger(EmailServiceImpl.class);

    private final JavaMailSender mailSender;
    private final EmailMessageRepository repo;

    @Value("${spring.mail.from}")
    private String defaultFrom;

    @Override
    @Transactional
    public EmailMessage send(EmailRequest request) {

        EmailMessage msg = EmailMessage.builder()
                .recipients(request.getTo())
                .subject(request.getSubject())
                .body(request.getBody())
                .status("QUEUED")
                .createdAt(LocalDateTime.now())
                .createdBy(request.getCreatedBy())
                .build();

        repo.save(msg);

        sendAsync(msg);

        return msg;
    }

    @Async("emailExecutor")
    public void sendAsync(EmailMessage msg) {

        try {
            MimeMessage mime = mailSender.createMimeMessage();
            MimeMessageHelper helper = new MimeMessageHelper(mime, true, "UTF-8");

            helper.setFrom(defaultFrom);
            helper.setTo(msg.getRecipients().toArray(new String[0]));
            helper.setSubject(msg.getSubject());

            // HTML body
            helper.setText(msg.getBody(), true);

            mailSender.send(mime);

            msg.setStatus("SENT");
            msg.setSentAt(LocalDateTime.now());

        } catch (Exception e) {
            msg.setStatus("FAILED");
            msg.setError(e.getMessage());
            log.error("Email send failed", e);
        }

        repo.save(msg);
    }
}