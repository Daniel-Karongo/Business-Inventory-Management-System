package com.IntegrityTechnologies.business_manager.security;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.model.UserSession;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository.UserSessionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
@RequiredArgsConstructor
public class BranchResolver {

    private final UserSessionRepository sessionRepository;

    /**
     * Hybrid Resolution Strategy:
     *
     * If branchId explicitly provided:
     *      → validate user has active session in that branch
     *      → return branchId
     *
     * If branchId null:
     *      → ADMIN → return null (global aggregation)
     *      → Others → return current session branch
     */
    public UUID resolveBranch(UUID branchIdFromRequest) {

        UUID userId = SecurityUtils.currentUserId();
        Role role = SecurityUtils.currentRole();

        if (userId == null) {
            throw new SecurityException("Unauthenticated user");
        }

        if (branchIdFromRequest != null) {

            boolean hasSession =
                    sessionRepository
                            .findAllByUserIdAndLogoutTimeIsNull(userId)
                            .stream()
                            .anyMatch(s -> s.getBranchId().equals(branchIdFromRequest));

            if (!hasSession) {
                throw new SecurityException("User has no active session in branch");
            }

            return branchIdFromRequest;
        }

        // branchId not provided

        if (role == Role.ADMIN) {
            return null; // GLOBAL
        }

        UserSession session =
                sessionRepository
                        .findAllByUserIdAndLogoutTimeIsNull(userId)
                        .stream()
                        .findFirst()
                        .orElseThrow(() ->
                                new SecurityException("No active branch session found"));

        return session.getBranchId();
    }
}