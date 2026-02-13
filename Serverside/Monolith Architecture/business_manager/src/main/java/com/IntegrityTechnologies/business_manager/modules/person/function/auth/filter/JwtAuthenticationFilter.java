package com.IntegrityTechnologies.business_manager.modules.person.function.auth.filter;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.security.CustomUserDetails;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.service.CustomUserDetailsService;
import com.IntegrityTechnologies.business_manager.modules.person.function.auth.util.JwtUtil;
import com.IntegrityTechnologies.business_manager.modules.person.function.rollcall.repository.UserSessionRepository;
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

    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain
    ) throws ServletException, IOException {

        String jwt = extractTokenFromCookie(request);

        if (jwt != null && SecurityContextHolder.getContext().getAuthentication() == null) {
            try {
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

            } catch (Exception ignored) {
                // Do NOT send response here
                // Just clear context and continue
                SecurityContextHolder.clearContext();
            }
        }

        filterChain.doFilter(request, response);
    }

    /* =====================================================
       COOKIE EXTRACTION (SINGLE SOURCE)
       ===================================================== */
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
                // Auth endpoints
                path.startsWith("/api/auth/login")
                        || path.startsWith("/api/auth/password-reset")

                        // MPESA
                        || path.equals("/api/payments/mpesa/stk/callback")
                        || path.startsWith("/api/payments/mpesa/c2b")

                        // Public branches
                        || (path.equals("/api/branches") && "GET".equals(method));
    }
}