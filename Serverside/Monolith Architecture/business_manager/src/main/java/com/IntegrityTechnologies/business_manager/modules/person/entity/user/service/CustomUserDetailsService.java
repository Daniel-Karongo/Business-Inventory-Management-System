package com.IntegrityTechnologies.business_manager.modules.person.entity.user.service;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.userdetails.*;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
public class CustomUserDetailsService implements UserDetailsService {

    private final UserRepository userRepository;

    @Autowired
    public CustomUserDetailsService(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    @Override
    public UserDetails loadUserByUsername(String identifier) throws UsernameNotFoundException {
        // Attempt to find an active (non-deleted) user first
        Optional<com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User> activeOpt = userRepository.findByUsernameAndDeletedFalse(identifier)
                .or(() -> userRepository.findByEmailElementIgnoreCaseAndDeletedFalse(identifier))
                .or(() -> userRepository.findByIdNumberAndDeletedFalse(identifier));

        if (activeOpt.isPresent()) {
            com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User activeUser = activeOpt.get();
            return org.springframework.security.core.userdetails.User.builder()
                    .username(activeUser.getUsername())
                    .password(activeUser.getPassword())
                    .roles(activeUser.getRole() != null ? activeUser.getRole().name() : "USER")
                    .build();
        }

        // If no active user found, check if a general user exists (may be deleted)
        Optional<com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User> generalOpt = userRepository.findByUsername(identifier)
                .or(() -> userRepository.findByEmailElementIgnoreCase(identifier))
                .or(() -> userRepository.findByIdNumber(identifier));

        if (generalOpt.isPresent()) {
            throw new UsernameNotFoundException(
                    "User identifier: " + identifier + " found but user is deleted"
            );
        }

        throw new UsernameNotFoundException("User not found with identifier: " + identifier);
    }
}