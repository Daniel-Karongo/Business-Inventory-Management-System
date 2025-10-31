package com.IntegrityTechnologies.business_manager.modules.user.service;

import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.user.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.userdetails.*;
import org.springframework.stereotype.Service;

@Service
public class CustomUserDetailsService implements UserDetailsService {

    private final UserRepository userRepository;

    @Autowired
    public CustomUserDetailsService(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    @Override
    public UserDetails loadUserByUsername(String identifier) throws UsernameNotFoundException {
        // Try to find user by username, email, or ID number
        User user = userRepository.findByUsername(identifier)
                .or(() -> userRepository.findByEmailAddress(identifier))
                .or(() -> userRepository.findByIdNumber(identifier))
                .orElseThrow(() -> new UsernameNotFoundException("User not found with identifier: " + identifier));

        // Build Spring Security UserDetails
        return org.springframework.security.core.userdetails.User.builder()
                .username(user.getUsername())
                .password(user.getPassword())
                .roles(user.getRole() != null ? user.getRole().name() : "USER")
                .build();
    }
}