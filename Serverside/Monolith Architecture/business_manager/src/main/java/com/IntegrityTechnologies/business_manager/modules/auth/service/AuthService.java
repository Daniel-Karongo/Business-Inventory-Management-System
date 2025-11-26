package com.IntegrityTechnologies.business_manager.modules.auth.service;

import com.IntegrityTechnologies.business_manager.exception.UserNotFoundException;
import com.IntegrityTechnologies.business_manager.modules.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.modules.auth.dto.AuthResponse;
import com.IntegrityTechnologies.business_manager.modules.auth.util.JwtUtil;
import com.IntegrityTechnologies.business_manager.modules.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.department.repository.DepartmentRepository;
import com.IntegrityTechnologies.business_manager.modules.rollcall.controller.RollcallController;
import com.IntegrityTechnologies.business_manager.modules.rollcall.model.LoginRollcallRequest;
import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.user.repository.UserRepository;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.*;
import org.springframework.security.core.Authentication;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class AuthService {

    private final AuthenticationManager authenticationManager;
    private final JwtUtil jwtUtil;
    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;
    private final TokenBlacklistService tokenBlacklistService;
    private final RollcallController rollcallController;
    private final DepartmentRepository departmentRepository;

//    @Autowired
//    public AuthService(AuthenticationManager authenticationManager,
//                       JwtUtil jwtUtil,
//                       UserRepository userRepository,
//                       PasswordEncoder passwordEncoder,
//                       TokenBlacklistService tokenBlacklistService) {
//        this.authenticationManager = authenticationManager;
//        this.jwtUtil = jwtUtil;
//        this.userRepository = userRepository;
//        this.passwordEncoder = passwordEncoder;
//        this.tokenBlacklistService = tokenBlacklistService;
//    }

    public AuthResponse login(AuthRequest request) {
        User user = userRepository.findByIdentifier(request.getIdentifier())
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        if (!passwordEncoder.matches(request.getPassword(), user.getPassword())) {
            throw new BadCredentialsException("Invalid password");
        }

        Authentication authentication = authenticationManager.authenticate(
                new UsernamePasswordAuthenticationToken(user.getUsername(), request.getPassword())
        );

        if (authentication.isAuthenticated()) {
            String token = jwtUtil.generateToken(user.getUsername());

            UUID userId = user.getId();
            List<Department> userDepartments = departmentRepository.findDepartmentsByUserId(userId);

            for(Department department: userDepartments) {
                LoginRollcallRequest req = new LoginRollcallRequest();
                req.setUserId(userId);
                req.setDepartmentId(department.getId());

                rollcallController.loginRollcall(req);
            }






            return new AuthResponse(token);
        } else {
            throw new RuntimeException("Authentication failed");
        }
    }

    public void logout(String token) {
        tokenBlacklistService.blacklistToken(token);
    }

    public boolean isTokenBlacklisted(String token) {
        return tokenBlacklistService.isTokenBlacklisted(token);
    }
}