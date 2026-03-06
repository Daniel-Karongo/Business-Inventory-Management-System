package com.IntegrityTechnologies.business_manager.security.auth.filter;

import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository.BranchRepository;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.security.CustomUserDetails;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.service.CustomUserDetailsService;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository.TenantRepository;
import com.IntegrityTechnologies.business_manager.security.BranchContext;
import com.IntegrityTechnologies.business_manager.security.auth.util.DeviceFingerprintUtil;
import com.IntegrityTechnologies.business_manager.security.auth.util.JwtUtil;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository.UserSessionRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.data.jpa.repository.support.SimpleJpaRepository;
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

    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain
    ) throws ServletException, IOException {

        String jwt = extractTokenFromCookie(request);

        if (jwt != null) {

            try {

                UUID tenantId = jwtUtil.extractTenantId(jwt);

                /* Verify tenant exists */
                if (!tenantRepository.existsById(tenantId)) {
                    throw new SecurityException("Invalid tenant in token");
                }

                TenantContext.setTenantId(tenantId);

                UUID branchId = jwtUtil.extractBranchId(jwt);

                boolean validBranch =
                        branchRepository.existsByIdAndTenantId(branchId, tenantId);

                if (!validBranch) {
                    throw new SecurityException("Invalid branch in token");
                }

                BranchContext.set(branchId);

                String tokenDevice = jwtUtil.extractDevice(jwt);
                String requestDevice = DeviceFingerprintUtil.generate(request);

                if (!tokenDevice.equals(requestDevice)) {
                    throw new SecurityException("Token device mismatch");
                }

                if (SecurityContextHolder.getContext().getAuthentication() == null) {

                    UUID tokenId = jwtUtil.extractTokenId(jwt);

                    boolean sessionValid =
                            userSessionRepository
                                    .findByTokenIdAndLogoutTimeIsNull(tokenId)
                                    .isPresent();

                    if (sessionValid) {

                        String username = jwtUtil.extractUsername(jwt);

                        CustomUserDetails userDetails =
                                (CustomUserDetails) userDetailsService.loadUserByUsername(username);

                        if (jwtUtil.validateToken(jwt, userDetails.getUsername())) {

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
                    }
                }

            } catch (Exception ignored) {

                SecurityContextHolder.clearContext();

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