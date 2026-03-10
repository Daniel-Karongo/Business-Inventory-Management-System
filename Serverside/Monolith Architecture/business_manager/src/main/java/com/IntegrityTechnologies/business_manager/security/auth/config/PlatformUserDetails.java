package com.IntegrityTechnologies.business_manager.security.auth.config;

import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformRole;
import lombok.Getter;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import java.util.Collection;
import java.util.List;
import java.util.UUID;

@Getter
public class PlatformUserDetails implements UserDetails {

    private final UUID id;
    private final String username;
    private final String password;
    private final PlatformRole role;
    private final boolean active;
    private final boolean locked;

    public PlatformUserDetails(
            UUID id,
            String username,
            String password,
            PlatformRole role,
            boolean active,
            boolean locked
    ) {
        this.id = id;
        this.username = username;
        this.password = password;
        this.role = role;
        this.active = active;
        this.locked = locked;
    }

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {

        return List.of(
                new SimpleGrantedAuthority("ROLE_" + role.name())
        );

    }

    @Override
    public boolean isAccountNonExpired() {
        return true;
    }

    @Override
    public boolean isAccountNonLocked() {
        return !locked;
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return true;
    }

    @Override
    public boolean isEnabled() {
        return active;
    }
}