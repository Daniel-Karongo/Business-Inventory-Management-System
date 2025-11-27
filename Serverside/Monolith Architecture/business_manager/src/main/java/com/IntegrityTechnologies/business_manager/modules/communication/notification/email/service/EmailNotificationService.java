package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.service;

import com.IntegrityTechnologies.business_manager.modules.person.function.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.Rollcall;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.env.Environment;
import org.springframework.mail.MailException;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.Set;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class EmailNotificationService implements NotificationService {

    private final JavaMailSender mailSender;
    private final Environment env;

    @Value("${spring.mail.from}")
    private String defaultFrom;

    @Override
    @Async("emailExecutor")
    public void notifyAbsent(Department dept, User absentUser, Rollcall absentRollcall) {

        if (dept == null) return;

        // collecting department heads' emails
        Set<String> to = dept.getHeads().stream()
                .flatMap(u -> u.getEmailAddresses() == null
                        ? java.util.stream.Stream.empty()
                        : u.getEmailAddresses().stream())
                .filter(e -> e != null && !e.isBlank())
                .collect(Collectors.toSet());

        if (to.isEmpty()) return;

        String subject = String.format("Rollcall Alert: %s absent from %s",
                absentUser.getUsername(), dept.getName());

        String body = String.format("""
                User %s (ID: %s) did not check in for department %s.
                Timestamp: %s
                Rollcall ID: %s
                """,
                absentUser.getUsername(),
                absentUser.getId(),
                dept.getName(),
                absentRollcall.getTimestamp(),
                absentRollcall.getId()
        );

        SimpleMailMessage msg = new SimpleMailMessage();
        msg.setFrom(defaultFrom);
        msg.setTo(to.toArray(new String[0]));
        msg.setSubject(subject);
        msg.setText(body);

        try {
            mailSender.send(msg);
            System.out.println("Email sent to: " + to);
        } catch (MailException e) {
            System.err.println("Failed to send rollcall notification: " + e.getMessage());
        }
    }
}