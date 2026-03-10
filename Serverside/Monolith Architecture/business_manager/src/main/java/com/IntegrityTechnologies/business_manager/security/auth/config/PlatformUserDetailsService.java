package com.IntegrityTechnologies.business_manager.security.auth.config;

import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUser;
import com.IntegrityTechnologies.business_manager.modules.platform.identity.repository.PlatformUserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class PlatformUserDetailsService implements UserDetailsService {

    private final PlatformUserRepository repository;

    @Override
    public UserDetails loadUserByUsername(String username)
            throws UsernameNotFoundException {

        PlatformUser user = repository
                .findByUsernameAndDeletedFalse(username)
                .orElseThrow(() ->
                        new UsernameNotFoundException("Platform user not found"));

        return new PlatformUserDetails(
                user.getId(),
                user.getUsername(),
                user.getPassword(),
                user.getRole(),
                user.isActive(),
                user.isLocked()
        );
    }
}