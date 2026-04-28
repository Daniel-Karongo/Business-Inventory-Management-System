package com.IntegrityTechnologies.business_manager.security.util;

import com.IntegrityTechnologies.business_manager.exception.UnauthorizedAccessException;
import com.IntegrityTechnologies.business_manager.modules.person.department.model.DepartmentMembershipRole;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.hibernate.Hibernate;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;

import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class PrivilegesChecker {
    private final UserRepository userRepository;

    public User getAuthenticatedUser(Authentication authentication) {
        if (authentication == null || !(authentication.getPrincipal() instanceof UserDetails userDetails)) {
            throw new UnauthorizedAccessException("You must be logged in to access this resource");
        }

        return userRepository.findByUsernameAndTenantIdAndDeletedFalse(userDetails.getUsername(), TenantContext.getTenantId())
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

        if (requester == null || target == null) {
            return false;
        }

        if (requester.getId().equals(target.getId())) {
            return true;
        }

        Role requesterRole = requester.getRole();

    /*
      Supervisors are now included in user management,
      but can only manage employees because canManage()
      is strictly greater-than.
    */
        boolean isManagerial =
                requesterRole == Role.SUPERUSER
                        || requesterRole == Role.ADMIN
                        || requesterRole == Role.MANAGER;

        if (!isManagerial) {
            return false;
        }

        return requesterRole.canManage(
                target.getRole()
        );
    }

    public boolean isAuthorizedWithinDepartment(
            User requester,
            User target
    ) {

        if (requester == null || target == null) {
            return false;
        }

        if (requester.getId().equals(target.getId())) {
            return true;
        }

        // Global managerial still works
        if (isAuthorized(requester, target)) {
            return true;
        }

        /*
          Department-head exception:
          Supervisor acting as HEAD may manage only
          EMPLOYEES inside same department.
        */

        if (requester.getRole() != Role.SUPERVISOR) {
            return false;
        }

        if (target.getRole() != Role.EMPLOYEE) {
            return false;
        }
        Hibernate.initialize(requester.getDepartments());
        Hibernate.initialize(target.getDepartments());

        Set<UUID> requesterHeadDepartments =
                requester.getDepartments()
                        .stream()
                        .filter(rel ->
                                rel.getRole()
                                        == DepartmentMembershipRole.HEAD
                        )
                        .map(rel ->
                                rel.getDepartment().getId()
                        )
                        .collect(Collectors.toSet());

        if (requesterHeadDepartments.isEmpty()) {
            return false;
        }

        boolean sharesDepartment =
                target.getDepartments()
                        .stream()
                        .anyMatch(rel ->
                                requesterHeadDepartments.contains(
                                        rel.getDepartment().getId()
                                )
                        );

        return sharesDepartment;
    }
}
