package com.IntegrityTechnologies.business_manager.security.auth.config;

import com.IntegrityTechnologies.business_manager.security.auth.service.RoutingUserDetailsService;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.crypto.password.PasswordEncoder;

@Configuration
@RequiredArgsConstructor
public class AuthenticationConfig {

    private final RoutingUserDetailsService routingUserDetailsService;
    private final PasswordEncoder passwordEncoder;

    @Bean
    public DaoAuthenticationProvider authenticationProvider() {

        DaoAuthenticationProvider provider = new DaoAuthenticationProvider();

        provider.setUserDetailsService(routingUserDetailsService);
        provider.setPasswordEncoder(passwordEncoder);

        return provider;
    }
}