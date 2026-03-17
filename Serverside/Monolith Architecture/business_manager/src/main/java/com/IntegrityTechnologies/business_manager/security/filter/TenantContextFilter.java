package com.IntegrityTechnologies.business_manager.security.filter;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.TenantStatus;
import com.IntegrityTechnologies.business_manager.security.util.HibernateTenantFilterManager;
import com.IntegrityTechnologies.business_manager.security.util.SecurityUtils;
import com.IntegrityTechnologies.business_manager.security.cache.TenantMetadataCache;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
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
public class TenantContextFilter extends OncePerRequestFilter {

    private final TenantMetadataCache tenantMetadataCache;
    private final HibernateTenantFilterManager filterManager;

    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain
    ) throws ServletException, IOException {

        try {

            /* =====================================================
               PLATFORM ADMIN BYPASS
            ===================================================== */
            if (SecurityUtils.isPlatformAdmin()) {
                filterChain.doFilter(request, response);
                return;
            }

            /* =====================================================
               TENANT RESOLUTION CHECK
            ===================================================== */
            UUID tenantId = TenantContext.getOrNull();

            if (tenantId == null) {
                filterChain.doFilter(request, response);
                return;
            }

            /* =====================================================
               TENANT STATUS VALIDATION
            ===================================================== */
            TenantStatus status =
                    tenantMetadataCache.getTenantStatus(tenantId);

            if (status != TenantStatus.ACTIVE
                    && status != TenantStatus.TRIAL) {

                throw new RuntimeException("Tenant inactive");
            }

            /* =====================================================
               ENABLE HIBERNATE TENANT FILTER (CRITICAL FIX)
            ===================================================== */
            if (tenantId != null)
                filterManager.enable(tenantId);

            /* =====================================================
               CONTINUE FILTER CHAIN
            ===================================================== */
            filterChain.doFilter(request, response);

        } finally {
            /* =====================================================
               CLEANUP THREAD CONTEXT
            ===================================================== */
            TenantContext.clear();
        }
    }
}