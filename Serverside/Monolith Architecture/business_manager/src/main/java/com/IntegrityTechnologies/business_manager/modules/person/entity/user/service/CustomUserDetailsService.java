package com.IntegrityTechnologies.business_manager.modules.person.entity.user.service;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.security.CustomUserDetails;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import org.springframework.security.core.userdetails.*;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class CustomUserDetailsService implements UserDetailsService {

    private final UserRepository userRepository;

    public CustomUserDetailsService(UserRepository userRepository) {
        this.userRepository = userRepository;
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

        return new CustomUserDetails(
                user.getId(),
                user.getTenantId(),
                null,
                user.getUsername(),
                user.getPassword(),
                user.getRole() != null
                        ? user.getRole()
                        : Role.EMPLOYEE,
                !Boolean.TRUE.equals(user.getDeleted())
        );
    }
}