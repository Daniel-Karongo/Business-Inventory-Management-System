package com.IntegrityTechnologies.business_manager.modules.auth.service;

import com.IntegrityTechnologies.business_manager.exception.UserNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.modules.auth.dto.AuthResponse;
import com.IntegrityTechnologies.business_manager.modules.auth.util.JwtUtil;
import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.user.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.*;
import org.springframework.security.core.Authentication;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

@Service
public class AuthService {

    private final AuthenticationManager authenticationManager;
    private final JwtUtil jwtUtil;
    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;

    @Autowired
    public AuthService(AuthenticationManager authenticationManager,
                       JwtUtil jwtUtil,
                       UserRepository userRepository,
                       PasswordEncoder passwordEncoder) {
        this.authenticationManager = authenticationManager;
        this.jwtUtil = jwtUtil;
        this.userRepository = userRepository;
        this.passwordEncoder = passwordEncoder;
    }

    public AuthResponse login(AuthRequest request) {
        // Try to find user by username, email, or ID number
        User user = userRepository.findByIdentifier(request.getIdentifier())
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        // Manually verify password before authentication manager
        if (!passwordEncoder.matches(request.getPassword(), user.getPassword())) {
            throw new BadCredentialsException("Invalid password");
        }

        // Authenticate through security context
        Authentication authentication = authenticationManager.authenticate(
                new UsernamePasswordAuthenticationToken(user.getUsername(), request.getPassword())
        );

        if (authentication.isAuthenticated()) {
            String token = jwtUtil.generateToken(user.getUsername());
            return new AuthResponse(token);
        } else {
            throw new RuntimeException("Authentication failed");
        }
    }
}