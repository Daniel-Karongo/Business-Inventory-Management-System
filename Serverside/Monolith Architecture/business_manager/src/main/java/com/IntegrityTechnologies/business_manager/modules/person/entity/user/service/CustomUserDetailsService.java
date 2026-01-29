package com.IntegrityTechnologies.business_manager.modules.person.entity.user.service;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository.UserRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.security.CustomUserDetails;
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

        var userOpt = userRepository.findByUsernameAndDeletedFalse(identifier)
                .or(() -> userRepository.findByEmailElementIgnoreCaseAndDeletedFalse(identifier))
                .or(() -> userRepository.findByIdNumberAndDeletedFalse(identifier));

        if (userOpt.isEmpty()) {
            throw new UsernameNotFoundException("User not found: " + identifier);
        }

        var user = userOpt.get();

        return new CustomUserDetails(
                user.getId(),
                user.getUsername(),
                user.getPassword(),
                user.getRole() != null ? user.getRole() : Role.EMPLOYEE,
                !Boolean.TRUE.equals(user.getDeleted())
        );
    }
}