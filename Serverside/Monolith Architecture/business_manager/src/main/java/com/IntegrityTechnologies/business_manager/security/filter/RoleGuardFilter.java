package com.IntegrityTechnologies.business_manager.security.filter;

import com.IntegrityTechnologies.business_manager.modules.platform.security.annotation.*;
import com.IntegrityTechnologies.business_manager.modules.platform.security.service.RoleGuardService;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.*;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerMapping;

import java.io.IOException;
import java.lang.annotation.Annotation;

@Slf4j
@Component
@RequiredArgsConstructor
public class RoleGuardFilter extends OncePerRequestFilter {

    private final RoleGuardService roleGuardService;

    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain chain
    ) throws ServletException, IOException {

        Object handler =
                request.getAttribute(HandlerMapping.BEST_MATCHING_HANDLER_ATTRIBUTE);

        if (!(handler instanceof HandlerMethod handlerMethod)) {
            chain.doFilter(request, response);
            return;
        }

        Class<?> controllerClass = handlerMethod.getBeanType();

    /*
       1️⃣ METHOD annotations override everything
    */
        Annotation[] methodAnnotations =
                handlerMethod.getMethod().getAnnotations();

        // 1️⃣ METHOD annotations
        for (Annotation ann : methodAnnotations) {

            if (isPublic(ann)) {
                chain.doFilter(request, response);
                return;
            }

            if (isRoleGuardAnnotation(ann)) {
                boolean allowed = roleGuardService.isAllowed(ann);
                if (!allowed) {
                    deny(response);
                    return;
                }
                chain.doFilter(request, response);
                return;
            }
        }

    /*
       2️⃣ CONTROLLER annotations apply to all endpoints
    */
        Annotation[] classAnnotations = controllerClass.getAnnotations();

        for (Annotation ann : classAnnotations) {

            if (isPublic(ann)) {
                chain.doFilter(request, response);
                return;
            }

            if (isRoleGuardAnnotation(ann)) {

                boolean allowed = roleGuardService.isAllowed(ann);

                if (!allowed) {
                    deny(response);
                    return;
                }

                chain.doFilter(request, response);
                return;
            }
        }

        /*
           3️⃣ No annotation anywhere → deny
        */
        log.warn("SECURITY DENY: {} {} | no role annotation found",
                request.getMethod(),
                request.getRequestURI());
        deny(response);
//        chain.doFilter(request, response);
    }

    private boolean isRoleGuardAnnotation(Annotation ann) {

        return ann instanceof PlatformSuperAdminOnly
                || ann instanceof PlatformAdminOnly
                || ann instanceof PlatformUserOrTenantManager
                || ann instanceof TenantSuperuserOnly
                || ann instanceof TenantAdminOnly
                || ann instanceof TenantManagerOnly
                || ann instanceof TenantSupervisorOnly
                || ann instanceof TenantUserOnly;
    }

    private boolean isPublic(Annotation ann) {
        return ann instanceof PublicEndpoint;
    }

    private void deny(HttpServletResponse response) throws IOException {

        response.setStatus(HttpServletResponse.SC_FORBIDDEN);
        response.setContentType("application/json");

        response.getWriter().write("""
        {
          "status":403,
          "error":"FORBIDDEN",
          "message":"Insufficient role privileges"
        }
        """);
    }
}