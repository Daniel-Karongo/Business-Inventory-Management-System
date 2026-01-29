package com.IntegrityTechnologies.business_manager.security.acl.aspect;

import com.IntegrityTechnologies.business_manager.security.acl.annotation.RequirePermission;
import com.IntegrityTechnologies.business_manager.security.acl.service.PermissionEvaluatorService;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Aspect
@Component
@RequiredArgsConstructor
public class PermissionAspect {

    private final PermissionEvaluatorService permissionEvaluator;
    private final HttpServletRequest request;

    @Before("@annotation(requirePermission)")
    public void enforcePermission(
            JoinPoint joinPoint,
            RequirePermission requirePermission
    ) {

        Authentication auth = SecurityContextHolder.getContext().getAuthentication();

        if (auth == null || !auth.isAuthenticated()) {
            throw new AccessDeniedException("Unauthenticated request");
        }

        String permissionCode = requirePermission.value();

        boolean allowed = permissionEvaluator.hasPermission(auth, permissionCode);

        if (!allowed) {
            throw new AccessDeniedException(
                    "Access denied — missing permission: " + permissionCode
            );
        }

        // Branch Scope Enforcement (optional)
        if (requirePermission.enforceBranch()) {

            UUID userId = extractUserId(auth);
            UUID branchId = extractBranchId(requirePermission.branchParam());

            if (branchId != null) {
                boolean hasBranchAccess =
                        permissionEvaluator.hasBranchAccess(userId, branchId);

                if (!hasBranchAccess) {
                    throw new AccessDeniedException(
                            "Access denied — branch scope violation"
                    );
                }
            }
        }
    }

    private UUID extractUserId(Authentication auth) {
        Object principal = auth.getPrincipal();

        // Works with your JWT user model
        try {
            var method = principal.getClass().getMethod("getId");
            Object id = method.invoke(principal);
            return UUID.fromString(id.toString());
        } catch (Exception ignored) {
            throw new AccessDeniedException("Cannot resolve user ID");
        }
    }

    private UUID extractBranchId(String paramName) {
        try {
            String value = request.getParameter(paramName);
            return value == null ? null : UUID.fromString(value);
        } catch (Exception ignored) {
            return null;
        }
    }
}