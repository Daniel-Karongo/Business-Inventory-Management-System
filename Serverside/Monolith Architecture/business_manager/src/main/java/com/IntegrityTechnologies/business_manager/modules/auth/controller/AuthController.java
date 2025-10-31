package com.IntegrityTechnologies.business_manager.modules.auth.controller;

import com.IntegrityTechnologies.business_manager.modules.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.modules.auth.dto.AuthResponse;
import com.IntegrityTechnologies.business_manager.modules.auth.service.AuthService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/auth")
public class AuthController {

    private final AuthService authService;

    @Autowired
    public AuthController(AuthService authService) {
        this.authService = authService;
    }

    @PostMapping("/login")
    public ResponseEntity<AuthResponse> login(@RequestBody AuthRequest request) {
        return ResponseEntity.ok(authService.login(request));
    }
}
