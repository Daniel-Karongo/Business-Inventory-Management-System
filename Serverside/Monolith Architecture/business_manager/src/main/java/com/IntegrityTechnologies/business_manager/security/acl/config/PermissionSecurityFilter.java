package com.IntegrityTechnologies.business_manager.security.acl.config;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import com.IntegrityTechnologies.business_manager.security.acl.entity.EndpointPermission;
import com.IntegrityTechnologies.business_manager.security.acl.service.PermissionEvaluatorService;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.Optional;

@Slf4j
@Component
@RequiredArgsConstructor
public class PermissionSecurityFilter extends OncePerRequestFilter {

    private final PermissionEvaluatorService evaluator;

    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain
    ) throws ServletException, IOException {

        String method = request.getMethod();
        String path = request.getServletPath();

        // Skip unauthenticated routes handled elsewhere
        if (shouldIgnore(path)) {
            filterChain.doFilter(request, response);
            return;
        }

        Role role = SecurityUtils.currentRole();

        // SUPERUSER bypass
        if (role == Role.SUPERUSER) {
            filterChain.doFilter(request, response);
            return;
        }

        Optional<EndpointPermission> match =
                evaluator.resolveEndpointPermission(method, path);

        // No permission registered → allow by default (configurable later)
        if (match.isEmpty()) {
            log.debug("⚠️ No ACL rule found for {} {}", method, path);
            filterChain.doFilter(request, response);
            return;
        }

        EndpointPermission endpointPermission = match.get();

        boolean allowed = evaluator.evaluatePermission(endpointPermission);

        if (!allowed) {
            deny(response, endpointPermission.getPermission().getCode());
            return;
        }

        filterChain.doFilter(request, response);
    }

    private void deny(HttpServletResponse response, String perm) throws IOException {
        response.setStatus(HttpServletResponse.SC_FORBIDDEN);
        response.setContentType("application/json");

        response.getWriter().write("""
        {
          "status": 403,
          "error": "FORBIDDEN",
          "message": "Missing permission: %s"
        }
        """.formatted(perm));
    }

    private boolean shouldIgnore(String path) {

        return path.startsWith("/swagger-ui")
                || path.startsWith("/v3/api-docs")
                || path.startsWith("/api/auth")
                || path.startsWith("/api/payments/mpesa")
                || path.startsWith("/api/notification/sms/dlr")
                || path.equals("/api/branches"); // public GET branch
    }
}