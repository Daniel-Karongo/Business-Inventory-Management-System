package com.IntegrityTechnologies.business_manager.security.auth.filter;

import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.security.CustomUserDetails;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.service.CustomUserDetailsService;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository.TenantRepository;
import com.IntegrityTechnologies.business_manager.security.BranchContext;
import com.IntegrityTechnologies.business_manager.security.auth.service.TokenBlacklistService;
import com.IntegrityTechnologies.business_manager.security.auth.util.DeviceFingerprintUtil;
import com.IntegrityTechnologies.business_manager.security.auth.util.JwtUtil;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository.UserSessionRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.IntegrityTechnologies.business_manager.security.cache.TenantMetadataCache;
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
    private final TenantRepository tenantRepository;
    private final BranchRepository branchRepository;
    private final TenantMetadataCache tenantMetadataCache;
    private final TokenBlacklistService tokenBlacklistService;
    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain
    ) throws ServletException, IOException {

        String jwt = extractTokenFromCookie(request);

        if (jwt != null) {

            try {
                if (tokenBlacklistService.isTokenBlacklisted(jwt)) {
                    throw new SecurityException("Token has been revoked");
                }

                var existingAuth = SecurityContextHolder.getContext().getAuthentication();

                if (existingAuth == null || !existingAuth.isAuthenticated()) {

                /* ==========================================
                   DEVICE FINGERPRINT CHECK
                ========================================== */

                    String tokenDevice = jwtUtil.extractDevice(jwt);
                    String requestDevice = DeviceFingerprintUtil.generate(request);

                    if (tokenDevice == null || !tokenDevice.equals(requestDevice)) {
                        throw new SecurityException("Token device mismatch");
                    }

                /* ==========================================
                   TENANT EXTRACTION
                ========================================== */

                    UUID tenantId = jwtUtil.extractTenantId(jwt);

                    if (!tenantMetadataCache.tenantExists(tenantId)) {
                        throw new SecurityException("Invalid tenant in token");
                    }

                /* ==========================================
                   BRANCH EXTRACTION
                ========================================== */

                    UUID branchId = jwtUtil.extractBranchId(jwt);

                    boolean branchValid =
                            tenantMetadataCache.branchExists(tenantId, branchId);

                    if (!branchValid) {
                        throw new SecurityException("Invalid branch for tenant");
                    }

                /* ==========================================
                   CONTEXT INITIALIZATION
                ========================================== */

                    TenantContext.clear();
                    BranchContext.clear();

                    TenantContext.setTenantId(tenantId);
                    BranchContext.set(branchId);

                /* ==========================================
                   USER RESOLUTION
                ========================================== */

                    String username = jwtUtil.extractUsername(jwt);

                    CustomUserDetails userDetails =
                            (CustomUserDetails) userDetailsService
                                    .loadUserByUsername(username);

                    if (!jwtUtil.validateToken(jwt, userDetails.getUsername())) {
                        throw new SecurityException("Invalid JWT");
                    }

                /* ==========================================
                   SESSION VALIDATION
                ========================================== */

                    UUID tokenId = jwtUtil.extractTokenId(jwt);

                    boolean sessionValid =
                            userSessionRepository
                                    .findByTokenIdAndLogoutTimeIsNull(tokenId)
                                    .isPresent();

                    if (!sessionValid) {
                        throw new SecurityException("Session expired or revoked");
                    }

                /* ==========================================
                   AUTHENTICATION
                ========================================== */

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

            }catch (Exception ex) {

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
                        || (path.equals("/api/branches") && "GET".equals(method));
    }
}