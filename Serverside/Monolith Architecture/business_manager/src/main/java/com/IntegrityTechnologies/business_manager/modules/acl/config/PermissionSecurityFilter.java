//package com.IntegrityTechnologies.business_manager.modules.acl.config;
//
//import com.IntegrityTechnologies.business_manager.modules.acl.entity.EndpointPermission;
//import com.IntegrityTechnologies.business_manager.modules.acl.service.PermissionEvaluatorService;
//import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
//import jakarta.servlet.FilterChain;
//import jakarta.servlet.ServletException;
//import jakarta.servlet.http.*;
//import lombok.RequiredArgsConstructor;
//import lombok.extern.slf4j.Slf4j;
//import org.springframework.security.core.Authentication;
//import org.springframework.stereotype.Component;
//import org.springframework.web.filter.OncePerRequestFilter;
//
//import java.io.IOException;
//import java.util.Optional;
//
//@Slf4j
//@Component
//@RequiredArgsConstructor
//public class PermissionSecurityFilter extends OncePerRequestFilter {
//
//    private final PermissionEvaluatorService evaluator;
//
//    @Override
//    protected void doFilterInternal(
//            HttpServletRequest request,
//            HttpServletResponse response,
//            FilterChain chain
//    ) throws ServletException, IOException {
//
//        String method = request.getMethod();
//        String path = request.getServletPath();
//
//        /*
//         * 1️⃣ Only protect API endpoints
//         */
//        if (!path.startsWith("/api/")) {
//            chain.doFilter(request, response);
//            return;
//        }
//
//        /*
//         * 2️⃣ Require authentication
//         */
//        Authentication authentication = SecurityUtils.currentAuthentication();
//
//        if (authentication == null || !authentication.isAuthenticated()) {
//            deny(response, "AUTH_REQUIRED");
//            return;
//        }
//
//        /*
//         * 3️⃣ Resolve endpoint permission
//         */
//        Optional<EndpointPermission> endpointPermission =
//                evaluator.resolveEndpointPermission(method, path);
//
//        /*
//         * 4️⃣ Fail-safe if endpoint not configured
//         */
//        if (endpointPermission.isEmpty()) {
//
//            log.warn(
//                    "ACL BLOCKED request: {} {} | no endpoint permission configured",
//                    method,
//                    path
//            );
//
//            deny(response, "ACL_ENDPOINT_NOT_REGISTERED");
//            return;
//        }
//
//        EndpointPermission ep = endpointPermission.get();
//
//        /*
//         * 5️⃣ Evaluate permission
//         */
//        boolean allowed = evaluator.evaluatePermission(ep);
//
//        if (!allowed) {
//
//            log.warn(
//                    "ACL DENIED {} {} | permission={}",
//                    method,
//                    path,
//                    ep.getPermission().getCode()
//            );
//
//            deny(response, ep.getPermission().getCode());
//            return;
//        }
//
//        chain.doFilter(request, response);
//    }
//
//    private void deny(HttpServletResponse response, String code) throws IOException {
//
//        response.setStatus(HttpServletResponse.SC_FORBIDDEN);
//        response.setContentType("application/json");
//
//        response.getWriter().write("""
//        {
//          "status":403,
//          "error":"FORBIDDEN",
//          "message":"Missing permission: %s"
//        }
//        """.formatted(code));
//    }
//}