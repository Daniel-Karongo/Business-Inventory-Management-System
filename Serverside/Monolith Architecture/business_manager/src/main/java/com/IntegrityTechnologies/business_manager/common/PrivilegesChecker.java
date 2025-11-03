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

        return userRepository.findByUsername(userDetails.getUsername())
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
        if (requester == null) return false;

        Role requesterRole = requester.getRole();
        Role targetRole = target.getRole();

        if (!requesterRole.canAccess(targetRole)) {
            return false;
        }

        return requester.getRole() == Role.SUPERUSER ||
                requester.getRole() == Role.ADMIN ||
                requester.getRole() == Role.MANAGER ||
                requester.getUsername().equalsIgnoreCase(target.getUsername());
    }
}
