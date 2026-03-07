package com.IntegrityTechnologies.business_manager.modules.platform.tenant.filter;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.TenantStatus;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository.TenantRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.resolver.TenantResolver;
import com.IntegrityTechnologies.business_manager.security.SecurityUtils;
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

    private final TenantRepository tenantRepository;

    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain
    ) throws ServletException, IOException {

        try {

            if (SecurityUtils.isPlatformAdmin()) {
                filterChain.doFilter(request, response);
                return;
            }

            UUID tenantId = TenantContext.getOrNull();

            if (tenantId == null) {
                throw new SecurityException("Tenant not resolved from token");
            }

            Tenant tenant = tenantRepository.findById(tenantId)
                    .orElseThrow(() -> new RuntimeException("Tenant not found"));

            if (tenant.getStatus() != TenantStatus.ACTIVE
                    && tenant.getStatus() != TenantStatus.TRIAL) {

                throw new RuntimeException("Tenant inactive");
            }

            filterChain.doFilter(request, response);

        } finally {
            TenantContext.clear();
        }
    }
}