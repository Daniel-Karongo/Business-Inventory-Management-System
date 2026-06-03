//package com.IntegrityTechnologies.business_manager.config.async;
//
//import com.IntegrityTechnologies.business_manager.modules.person.branch.model.Branch;
//import com.IntegrityTechnologies.business_manager.modules.person.branch.repository.BranchRepository;
//import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.model.UserSession;
//import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.repository.UserSessionRepository;
//import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.service.RollcallService;
//import com.IntegrityTechnologies.business_manager.modules.platform.tenant.service.TenantExecutionService;
//import lombok.RequiredArgsConstructor;
//import org.springframework.beans.factory.annotation.Value;
//import org.springframework.scheduling.annotation.Scheduled;
//import org.springframework.stereotype.Component;
//
//import java.time.LocalDateTime;
//import java.time.LocalTime;
//import java.util.List;
//import java.util.Set;
//import java.util.UUID;
//
//@Component
//@RequiredArgsConstructor
//public class AutoLogoutScheduler {
//
//    private final LogoutCronProvider cronProvider;
//    private final LogoutRunTracker runTracker;
//    private final UserSessionRepository userSessionRepository;
//    private final BranchRepository branchRepository;
//    private final RollcallService rollcallService;
//    private final TenantExecutionService tenantExecutionService;
//
//    @Value("${rollcall.logout.start-time}")
//    private String startTimeString;
//
//    @Scheduled(cron="0 * * * * *")
//    public void autoLogoutMissingUsers() {
//        LocalTime startTime = LocalTime.parse(startTimeString);
//        LocalTime nowTime = LocalTime.now();
//
//        if (nowTime.isBefore(startTime)) {
//            return;
//        }
//
//        if (runTracker.hasRunToday()) {
//            System.out.println("Auto logout already executed today. Skipping...");
//            return;
//        }
//
//        LocalDateTime now = LocalDateTime.now();
//
//        tenantExecutionService.forEachTenant(tenantId -> {
//
//            List<UserSession> sessions =
//                    userSessionRepository.findByTenantIdAndLogoutTimeIsNull(tenantId);
//
//            for (UserSession session : sessions) {
//
//                UUID userId = session.getUserId();
//                UUID branchId = session.getBranchId();
//
//                Branch branch = branchRepository
//                        .findByTenantIdAndIdAndDeletedFalse(tenantId, branchId)
//                        .orElseThrow();
//
//                boolean shouldLogout = false;
//
//                LocalTime logoutTime = resolveLogoutTime(branch);
//
//                if (!nowTime.isBefore(logoutTime)) {
//                    shouldLogout = true;
//                }
//
//                if (!shouldLogout) continue;
//
//                if (session.getLogoutTime() != null) continue;
//
//                // ✅ logout ONCE
//                session.setLogoutTime(now);
//                session.setAutoLoggedOut(true);
//                userSessionRepository.save(session);
//
//                rollcallService.recordLogoutRollcall(userId, branchId);
//            }
//        });
//
//        runTracker.markRunToday();
//    }
//
//    private LocalTime resolveLogoutTime(Branch branch) {
//
//        if (branch.getLogoutTime() != null) {
//            return branch.getLogoutTime();
//        }
//
//        return LocalTime.parse(cronProvider.getConfiguredTime());
//    }
//}