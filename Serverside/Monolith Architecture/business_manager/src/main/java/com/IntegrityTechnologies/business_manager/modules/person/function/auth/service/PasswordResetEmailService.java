package com.IntegrityTechnologies.business_manager.modules.person.function.auth.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.dto.EmailRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.service.EmailService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class PasswordResetEmailService {

    private final EmailService emailService;

    public void sendResetEmail(User user, String otp) {

        if (user.getEmailAddresses() == null || user.getEmailAddresses().isEmpty()) {
            return; // silent fail (anti-account enumeration)
        }

        String email = user.getEmailAddresses().get(0);

        EmailRequest request = new EmailRequest();
        request.setTo(List.of(email));
        request.setSubject("Password Reset Code");
        request.setBody("""
            <html>
            <body style="font-family: Arial, sans-serif;">
                <p>Hello <strong>%s</strong>,</p>
            
                <p>Your password reset code is:</p>
            
                <h2 style="letter-spacing: 2px;">%s</h2>
            
                <p>
                    This code expires in <strong>15 minutes</strong>.
                </p>
            
                <p style="color: #666;">
                    If you did not request this reset, you can safely ignore this email.
                </p>
            </body>
            </html>
            """.formatted(
                user.getUsername(),
                otp
        ));

        request.setCreatedBy("SYSTEM");

        emailService.send(request);
    }
}