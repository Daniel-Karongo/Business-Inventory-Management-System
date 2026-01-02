package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.service;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class PasswordResetEmailService {

    private final JavaMailSender mailSender;

    @Value("${spring.mail.from}")
    private String from;

    @Value("${app.frontend.reset-password-url:http://localhost:4200/auth/reset-password}")
    private String resetUrl;

    public void sendResetEmail(User user, String rawToken) {

        if (user.getEmailAddresses() == null || user.getEmailAddresses().isEmpty()) {
            return; // fail silently to avoid account enumeration
        }

        String link = resetUrl + "?token=" + rawToken;

        SimpleMailMessage msg = new SimpleMailMessage();
        msg.setFrom(from);
        msg.setTo(user.getEmailAddresses().get(0));
        msg.setSubject("Password Reset Request");
        msg.setText("""
            Hello %s,

            A password reset was requested for your account.

            Click the link below to reset your password:
            %s

            If you did not request this, please ignore this email.

            This link expires automatically.
            """.formatted(user.getUsername(), link));

        mailSender.send(msg);
    }
}