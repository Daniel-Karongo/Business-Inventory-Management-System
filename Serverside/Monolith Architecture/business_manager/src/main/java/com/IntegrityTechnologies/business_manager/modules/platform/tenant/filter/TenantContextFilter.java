package com.IntegrityTechnologies.business_manager.modules.platform.tenant.filter;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.TenantStatus;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository.TenantRepository;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.resolver.TenantResolver;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

@Component
@RequiredArgsConstructor
public class TenantContextFilter extends OncePerRequestFilter {

    private final TenantResolver tenantResolver;
    private final TenantRepository tenantRepository;

    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain
    ) throws ServletException, IOException {

        try {

            String tenantCode = tenantResolver.resolveTenantCode(request);

            if (tenantCode == null) {
                throw new RuntimeException("Missing tenant header");
            }

            Tenant tenant = tenantRepository
                    .findByCode(tenantCode)
                    .orElseThrow(() -> new RuntimeException("Tenant not found"));

            if (tenant.getStatus() != TenantStatus.ACTIVE
                    && tenant.getStatus() != TenantStatus.TRIAL) {

                throw new RuntimeException("Tenant inactive");
            }

            TenantContext.setTenantId(tenant.getId());

            filterChain.doFilter(request, response);

        } finally {
            TenantContext.clear();
        }
    }
}