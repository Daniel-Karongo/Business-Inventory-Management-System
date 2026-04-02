package com.IntegrityTechnologies.business_manager.security.filter;

import com.IntegrityTechnologies.business_manager.security.auth.config.CustomUserDetails;
import com.IntegrityTechnologies.business_manager.security.auth.config.CustomUserDetailsService;
import com.IntegrityTechnologies.business_manager.modules.person.system.rollcall.repository.UserSessionRepository;
import com.IntegrityTechnologies.business_manager.security.auth.config.PlatformUserDetails;
import com.IntegrityTechnologies.business_manager.security.util.BranchContext;
import com.IntegrityTechnologies.business_manager.security.auth.config.PlatformUserDetailsService;
import com.IntegrityTechnologies.business_manager.security.auth.service.TokenBlacklistService;
import com.IntegrityTechnologies.business_manager.security.auth.util.DeviceFingerprintUtil;
import com.IntegrityTechnologies.business_manager.security.auth.util.JwtUtil;
import com.IntegrityTechnologies.business_manager.security.cache.TenantMetadataCache;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class JwtAuthenticationFilter extends OncePerRequestFilter {

    private final JwtUtil jwtUtil;
    private final CustomUserDetailsService userDetailsService;
    private final UserSessionRepository userSessionRepository;
    private final TenantMetadataCache tenantMetadataCache;
    private final TokenBlacklistService tokenBlacklistService;
    private final PlatformUserDetailsService platformUserDetailsService;
    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain
    ) throws ServletException, IOException {

        String jwt = extractTokenFromCookie(request);

        if (jwt != null && SecurityContextHolder.getContext().getAuthentication() == null) {

            try {

                if (tokenBlacklistService.isTokenBlacklisted(jwt)) {
                    throw new SecurityException("Token revoked");
                }

                var existingAuth = SecurityContextHolder.getContext().getAuthentication();

                if (existingAuth == null || !existingAuth.isAuthenticated()) {

                /* ==========================================
                   DEVICE FINGERPRINT CHECK
                ========================================== */

                    String tokenDevice = jwtUtil.extractDevice(jwt);
                    String deviceId = request.getHeader("X-Device-Id");

                    if (deviceId == null || deviceId.isBlank()) {
                        throw new SecurityException("Missing device ID");
                    }

                    String requestDevice =
                            DeviceFingerprintUtil.generate(request, deviceId);

                    if (tokenDevice == null || !tokenDevice.equals(requestDevice)) {
                        throw new SecurityException("Token device mismatch");
                    }

                /* ==========================================
                   USER TYPE
                ========================================== */

                    String userType = jwtUtil.extractUserType(jwt);

                /* =====================================================
                   PLATFORM USER FLOW
                ===================================================== */

                    if ("PLATFORM".equals(userType)) {

                        String username = jwtUtil.extractUsername(jwt);

                        PlatformUserDetails userDetails =
                                (PlatformUserDetails) platformUserDetailsService
                                        .loadUserByUsername(username);

                        if (!jwtUtil.validateToken(jwt, userDetails.getUsername())) {
                            throw new SecurityException("Invalid JWT");
                        }

                        UsernamePasswordAuthenticationToken authToken =
                                new UsernamePasswordAuthenticationToken(
                                        userDetails,
                                        null,
                                        userDetails.getAuthorities()
                                );

                        authToken.setDetails(
                                new WebAuthenticationDetailsSource().buildDetails(request)
                        );

                        SecurityContextHolder
                                .getContext()
                                .setAuthentication(authToken);

                        filterChain.doFilter(request, response);
                        return;
                    }

                /* =====================================================
                   TENANT USER FLOW
                ===================================================== */

                    UUID tenantId = jwtUtil.extractTenantId(jwt);

                    if (!tenantMetadataCache.tenantExists(tenantId)) {
                        throw new SecurityException("Invalid tenant");
                    }

                    UUID branchId = jwtUtil.extractBranchId(jwt);

                    boolean branchValid =
                            tenantMetadataCache.branchExists(tenantId, branchId);

                    if (!branchValid) {
                        throw new SecurityException("Invalid branch");
                    }

                    UUID currentTenant = TenantContext.getOrNull();

                    if (currentTenant == null) {
                        TenantContext.setTenantId(tenantId);
                    } else if (!currentTenant.equals(tenantId)) {
                        throw new SecurityException("Tenant mismatch between request and token");
                    }

                    BranchContext.set(branchId);

                    String username = jwtUtil.extractUsername(jwt);

                    CustomUserDetails userDetails =
                            (CustomUserDetails) userDetailsService
                                    .loadUserByUsername(username);

                    if (!jwtUtil.validateToken(jwt, userDetails.getUsername())) {
                        throw new SecurityException("Invalid JWT");
                    }

                    UUID tokenId = jwtUtil.extractTokenId(jwt);

                    boolean sessionValid =
                            userSessionRepository
                                    .findByTokenIdAndLogoutTimeIsNull(tokenId)
                                    .isPresent();

                    if (!sessionValid) {
                        throw new SecurityException("Session expired");
                    }

                    UsernamePasswordAuthenticationToken authToken =
                            new UsernamePasswordAuthenticationToken(
                                    userDetails,
                                    null,
                                    userDetails.getAuthorities()
                            );

                    authToken.setDetails(
                            new WebAuthenticationDetailsSource()
                                    .buildDetails(request)
                    );

                    SecurityContextHolder
                            .getContext()
                            .setAuthentication(authToken);
                }

            } catch (Exception ex) {

                ex.printStackTrace();   // ADD THIS

                SecurityContextHolder.clearContext();
                TenantContext.clear();
                BranchContext.clear();

                request.setAttribute("jwt_error", ex.getMessage());
            }
        }

        filterChain.doFilter(request, response);
    }

    private String extractTokenFromCookie(HttpServletRequest request) {

        Cookie[] cookies = request.getCookies();

        if (cookies == null) return null;

        for (Cookie c : cookies) {
            if ("access_token".equals(c.getName())) {
                return c.getValue();
            }
        }

        return null;
    }

    @Override
    protected boolean shouldNotFilter(HttpServletRequest request) {

        String path = request.getServletPath();
        String method = request.getMethod();

        return
                path.startsWith("/api/auth/login")
                        || path.startsWith("/api/auth/password-reset")
                        || path.equals("/api/payments/mpesa/stk/callback")
                        || path.startsWith("/api/payments/mpesa/c2b")
                        || path.equals("/api/platform/settings/logo")
                        || (path.equals("/api/branches") && "GET".equals(method));
    }
}