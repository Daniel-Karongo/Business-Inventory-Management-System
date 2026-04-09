package com.IntegrityTechnologies.business_manager.security.auth.orchestrator;

import com.IntegrityTechnologies.business_manager.security.auth.dto.AuthRequest;
import com.IntegrityTechnologies.business_manager.security.auth.service.AuthService;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class AuthOrchestratorService {

    private final AuthService authService;

    /* =====================================================
       PASSWORD LOGIN (UNCHANGED BEHAVIOR)
    ===================================================== */
    public AuthService.LoginResult login(
            AuthRequest request,
            HttpServletRequest httpRequest
    ) {
        // Delegates FULLY to existing logic
        return authService.loginInternal(request, httpRequest);
    }

    /* =====================================================
       BIOMETRIC LOGIN (UNCHANGED BEHAVIOR)
    ===================================================== */
    public AuthService.LoginResult biometricLogin(
            AuthRequest request,
            HttpServletRequest httpRequest
    ) {
        return authService.loginInternalBiometric(request, httpRequest);
    }
}