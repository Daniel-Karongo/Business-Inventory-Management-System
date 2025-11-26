package com.IntegrityTechnologies.business_manager.modules.notification.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequiredArgsConstructor
public class EmailTestController {

    private final JavaMailSender mailSender;

    @Value("${spring.mail.from}")
    private String defaultFrom;

    @GetMapping("/notifications/test-email")
    public String testEmail(@RequestParam String to) {
        try {
            SimpleMailMessage msg = new SimpleMailMessage();
            msg.setFrom(defaultFrom);
            msg.setTo(to);
            msg.setSubject("Test Email from Business Manager System");
            msg.setText("If you received this, your SMTP configuration is working.");

            mailSender.send(msg);
            return "Email successfully sent to " + to;

        } catch (Exception e) {
            return "Failed to send email: " + e.getMessage();
        }
    }
}