package com.IntegrityTechnologies.business_manager.modules.platform.security.rate;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class TenantRateLimitFilter extends OncePerRequestFilter {

    private final TenantRateLimiter limiter;

    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain
    ) throws ServletException, IOException {

        UUID tenantId = TenantContext.getOrNull();

        if (tenantId != null) {

            boolean allowed = limiter.allow(tenantId);

            if (!allowed) {

                response.setStatus(429);
                response.setContentType("application/json");

                response.getWriter().write("""
                    {
                      "status":429,
                      "error":"Too Many Requests",
                      "message":"Tenant request limit exceeded"
                    }
                """);

                return;
            }
        }

        filterChain.doFilter(request, response);
    }
}