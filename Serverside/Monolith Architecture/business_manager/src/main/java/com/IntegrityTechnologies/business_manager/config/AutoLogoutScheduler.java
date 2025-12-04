package com.IntegrityTechnologies.business_manager.config;

import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.UserSession;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository.UserSessionRepository;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.service.RollcallService;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class AutoLogoutScheduler {

    private final LogoutCronProvider cronProvider;
    private final LogoutRunTracker runTracker;
    private final UserSessionRepository userSessionRepository;
    private final DepartmentRepository departmentRepository;
    private final BranchRepository branchRepository;
    private final RollcallService rollcallService;

    @Scheduled(cron = "#{@logoutCronProvider.getCron()}")
    @Transactional
    public void autoLogoutMissingUsers() {

        // ---- PREVENT MULTIPLE RUNS TODAY ----
        if (runTracker.hasRunToday()) {
            System.out.println("Auto logout already executed today. Skipping...");
            return;
        }

        LocalDateTime now = LocalDateTime.now();
        List<UserSession> sessions = userSessionRepository.findAllByLogoutTimeIsNull();

        for (UserSession session : sessions) {

            session.setLogoutTime(now);
            session.setAutoLoggedOut(true);
            userSessionRepository.save(session);

            UUID userId = session.getUserId();
            UUID branchId = session.getBranchId();

            Set<Department> departments = departmentRepository.findDepartmentsByUserId(userId);

            if (departments.isEmpty()) {
                rollcallService.recordLogoutRollcall(userId, null, branchId);
            } else {
                for (Department dept : departments) {
                    rollcallService.recordLogoutRollcall(userId, dept.getId(), branchId);
                }
            }
        }

        System.out.println("Auto logout executed at: " + now);

        // ---- MARK AS EXECUTED FOR TODAY ----
        runTracker.markRunToday();
    }
}