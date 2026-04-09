package com.IntegrityTechnologies.business_manager.security.filter;

import com.IntegrityTechnologies.business_manager.security.cache.TenantMetadataCache;
import com.IntegrityTechnologies.business_manager.security.util.*;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.UUID;

@Component
@RequiredArgsConstructor
public class BranchContextFilter extends OncePerRequestFilter {

    private final BranchResolver branchResolver;
    private final HibernateBranchFilterManager branchFilterManager;
    private final TenantMetadataCache tenantMetadataCache;

    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain
    ) throws ServletException, IOException {

        UUID tenantId = TenantContext.getOrNull();

        if (tenantId != null && tenantMetadataCache.isPlatformTenant(tenantId)) {
            filterChain.doFilter(request, response);
            return;
        }

        try {
            if (SecurityUtils.isPlatformAdmin()) {
                filterChain.doFilter(request, response);
                return;
            }

            Authentication auth =
                    SecurityContextHolder.getContext().getAuthentication();

            // Skip branch resolution for unauthenticated users
            if (auth == null || !auth.isAuthenticated()) {
                filterChain.doFilter(request, response);
                return;
            }

            String branchParam = request.getParameter("branchId");

            UUID branchId = null;

            if (branchParam != null && !branchParam.isBlank()) {
                branchId = UUID.fromString(branchParam);
            }

            UUID resolved = branchResolver.resolveBranch(branchId);

            // 👇 THIS IS THE KEY CHANGE
            BranchContext.set(resolved);

            // 🔥 Apply filter ONLY if branch is NOT null
            branchFilterManager.enable(resolved);

            filterChain.doFilter(request, response);

        } finally {
            BranchContext.clear();
        }
    }
}