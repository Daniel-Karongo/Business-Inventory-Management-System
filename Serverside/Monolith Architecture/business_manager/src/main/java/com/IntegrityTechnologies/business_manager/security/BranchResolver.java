package com.IntegrityTechnologies.business_manager.security;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserBranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.UserSession;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository.UserSessionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
@RequiredArgsConstructor
public class BranchResolver {

    private final UserSessionRepository sessionRepository;
    private final UserBranchRepository userBranchRepository;

    public UUID resolveBranch(UUID branchIdFromRequest) {

        UUID userId = SecurityUtils.currentUserId();
        Role role = SecurityUtils.currentRole();

        if (userId == null) {
            throw new SecurityException("Unauthenticated user");
        }

        /* ---------- Explicit branch ---------- */

        if (branchIdFromRequest != null) {

            boolean hasSession =
                    sessionRepository.existsByUserIdAndBranchIdAndLogoutTimeIsNull(
                            userId,
                            branchIdFromRequest
                    );

            if (!hasSession) {
                throw new SecurityException("User has no active session in branch");
            }

            boolean member =
                    userBranchRepository.existsByUser_IdAndBranch_Id(
                            userId,
                            branchIdFromRequest
                    );

            if (!member) {
                throw new SecurityException("User not assigned to branch");
            }

            return branchIdFromRequest;
        }

        /* ---------- Admin global access ---------- */

        if (role.canAccess(Role.MANAGER)) {
            return null;
        }

        /* ---------- Resolve current session ---------- */

        UserSession session =
                sessionRepository
                        .findFirstByUserIdAndLogoutTimeIsNull(userId)
                        .orElseThrow(() ->
                                new SecurityException("No active branch session"));

        UUID branchId = session.getBranchId();

        boolean member =
                userBranchRepository.existsByUser_IdAndBranch_Id(
                        userId,
                        branchId
                );

        if (!member) {
            throw new SecurityException("User not assigned to branch");
        }

        return branchId;
    }
}