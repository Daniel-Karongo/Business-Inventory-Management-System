package com.IntegrityTechnologies.business_manager.security.auth.service;

import com.IntegrityTechnologies.business_manager.security.auth.config.CustomUserDetailsService;
import com.IntegrityTechnologies.business_manager.security.auth.config.PlatformUserDetailsService;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Primary;
import org.springframework.security.core.userdetails.*;
import org.springframework.stereotype.Service;

@Service
@Primary
@RequiredArgsConstructor
public class RoutingUserDetailsService implements UserDetailsService {

    private final CustomUserDetailsService tenantService;
    private final PlatformUserDetailsService platformService;

    @Override
    public UserDetails loadUserByUsername(String username)
            throws UsernameNotFoundException {

        if (TenantContext.getTenantId() != null) {
            return tenantService.loadUserByUsername(username);
        }

        return platformService.loadUserByUsername(username);
    }
}