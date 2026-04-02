package com.IntegrityTechnologies.business_manager.security.auth.config;

import com.IntegrityTechnologies.business_manager.modules.person.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.person.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.repository.UserSessionRepository;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import org.springframework.security.core.userdetails.*;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class CustomUserDetailsService implements UserDetailsService {

    private final UserRepository userRepository;
    private final UserSessionRepository userSessionRepository;

    public CustomUserDetailsService(
            UserRepository userRepository,
            UserSessionRepository userSessionRepository
    ) {
        this.userRepository = userRepository;
        this.userSessionRepository = userSessionRepository;
    }

    @Override
    public UserDetails loadUserByUsername(String identifier)
            throws UsernameNotFoundException {

        UUID tenantId = TenantContext.getTenantId();

        var user = userRepository
                .findAuthUser(tenantId, identifier)
                .orElseThrow(() ->
                        new UsernameNotFoundException(
                                "User not found: " + identifier
                        )
                );

        UUID branchId =
                userSessionRepository
                        .findFirstByUserIdAndLogoutTimeIsNull(user.getId())
                        .map(s -> s.getBranchId())
                        .orElse(null);

        return new CustomUserDetails(
                user.getId(),
                user.getTenantId(),
                branchId,
                user.getUsername(),
                user.getPassword(),
                user.getRole() != null
                        ? user.getRole()
                        : Role.EMPLOYEE,
                !Boolean.TRUE.equals(user.getDeleted())
        );
    }
}