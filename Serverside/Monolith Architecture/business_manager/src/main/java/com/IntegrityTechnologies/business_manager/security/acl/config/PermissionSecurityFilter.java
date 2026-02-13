package com.IntegrityTechnologies.business_manager.security.acl.config;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
import com.IntegrityTechnologies.business_manager.security.acl.entity.EndpointPermission;
import com.IntegrityTechnologies.business_manager.security.acl.service.PermissionEvaluatorService;
import jakarta.servlet.*;
import jakarta.servlet.http.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.Optional;

@Component
@RequiredArgsConstructor
public class PermissionSecurityFilter extends OncePerRequestFilter {

    private final PermissionEvaluatorService evaluator;

    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain chain
    ) throws ServletException, IOException {

        String method = request.getMethod();
        String path = request.getServletPath();

        // 🔥 Only apply ACL to API endpoints
        if (!request.getRequestURI().startsWith("/api/")) {
            chain.doFilter(request, response);
            return;
        }

        System.out.println("ACL PATH: " + path);

        Role role = SecurityUtils.currentRole();

        Optional<EndpointPermission> ep =
                evaluator.resolveEndpointPermission(method, path);

        if (ep.isEmpty()) {
            deny(response, "ACL_MISSING");
            return;
        }

        if (!evaluator.evaluatePermission(ep.get())) {
            deny(response, ep.get().getPermission().getCode());
            return;
        }

        chain.doFilter(request, response);
    }

    private void deny(HttpServletResponse response, String code) throws IOException {
        response.setStatus(HttpServletResponse.SC_FORBIDDEN);
        response.setContentType("application/json");
        response.getWriter().write("""
        {
          "status":403,
          "error":"FORBIDDEN",
          "message":"Missing permission: %s"
        }
        """.formatted(code));
    }
}