package com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.dto.EmailRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.service.EmailService;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.DepartmentMembershipRole;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserDepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.Rollcall;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class RollcallEmailNotifier {

    private final EmailService emailService;
    private final UserDepartmentRepository userDepartmentRepository;

    public void notifyAbsent(
            Department dept,
            User absentUser,
            Rollcall rollcall
    ) {

        if (dept == null) return;

        List<String> emails =
                userDepartmentRepository.findByDepartmentIdWithUser(TenantContext.getTenantId(), dept.getId())
                        .stream()
                        .filter(r -> r.getRole() == DepartmentMembershipRole.HEAD)
                        .flatMap(r -> r.getUser().getEmailAddresses().stream())
                        .filter(e -> e != null && !e.isBlank())
                        .toList();

        if (emails.isEmpty()) return;

        EmailRequest req = new EmailRequest();
        req.setTo(emails);
        req.setSubject(
                "Rollcall Alert: " + absentUser.getUsername() + " absent"
        );
        req.setBody("""
                User %s (ID: %s) did not check in.

                Department: %s
                Time: %s
                Rollcall ID: %s
                """.formatted(
                absentUser.getUsername(),
                absentUser.getId(),
                dept.getName(),
                rollcall.getTimestamp(),
                rollcall.getId()
        ));
        req.setCreatedBy("SYSTEM");

        emailService.send(req);
    }
}