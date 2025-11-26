package com.IntegrityTechnologies.business_manager.common;

import com.IntegrityTechnologies.business_manager.exception.UnauthorizedAccessException;
import com.IntegrityTechnologies.business_manager.modules.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class PrivilegesChecker {
    private final UserRepository userRepository;

    public User getAuthenticatedUser(Authentication authentication) {
        if (authentication == null || !(authentication.getPrincipal() instanceof UserDetails userDetails)) {
            throw new UnauthorizedAccessException("You must be logged in to access this resource");
        }

        return userRepository.findByUsernameAndDeletedFalse(userDetails.getUsername())
                .orElseThrow(() -> new UnauthorizedAccessException("Authenticated user not found"));
    }

    public Role getCurrentUserRole() {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        return auth.getAuthorities().stream()
                .map(a -> Role.valueOf(a.getAuthority().replace("ROLE_", "")))
                .findFirst()
                .orElse(Role.EMPLOYEE); // default to lowest role if none found
    }

    public boolean isAuthorized(User requester, User target) {
        if (requester == null || target == null) return false;

        Role requesterRole = requester.getRole();
        Role targetRole = target.getRole();

        // 1️⃣ Self-access is always allowed
        if (requester.getUsername().equalsIgnoreCase(target.getUsername())) {
            return true;
        }

        // 2️⃣ Only managerial roles can access others
        boolean isManagerial = requesterRole == Role.SUPERUSER ||
                requesterRole == Role.ADMIN ||
                requesterRole == Role.MANAGER;
        if (!isManagerial) return false;

        // 3️⃣ Managerial roles can only access users with lower or equal role levels
        return requesterRole.canAccess(targetRole);
    }

    public boolean isAuthorizedToBeHead(User requester) {
        if (requester == null) return false;

        Role requesterRole = requester.getRole();

        boolean isManagerial = requesterRole == Role.SUPERUSER ||
                requesterRole == Role.ADMIN ||
                requesterRole == Role.MANAGER ||
                requesterRole == Role.SUPERVISOR;

        if (!isManagerial) return false;

        return true;
    }
}
