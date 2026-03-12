package com.IntegrityTechnologies.business_manager.modules.platform.tenant.filter;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository.TenantRepository;
import com.IntegrityTechnologies.business_manager.security.cache.TenantMetadataCache;
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
import java.util.UUID;

@Slf4j
@Component
@RequiredArgsConstructor
public class TenantResolutionFilter extends OncePerRequestFilter {

    private final TenantMetadataCache tenantMetadataCache;

    private static final String TENANT_HEADER = "X-Tenant";

    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain
    ) throws ServletException, IOException {
        String tenantCode = resolveTenant(request);

        if (tenantCode != null) {

            UUID tenantId =
                    tenantMetadataCache.getTenantIdByCode(tenantCode);

            TenantContext.setTenantId(tenantId);
        }
        filterChain.doFilter(request, response);
    }

    private String resolveTenant(HttpServletRequest request) {

        // 1️⃣ Header override
        String header = request.getHeader(TENANT_HEADER);
        if (header != null && !header.isBlank()) {
            return header;
        }

        // 2️⃣ Subdomain resolution
        String host = request.getServerName();

        if (host != null && host.contains(".")) {

            String subdomain = host.split("\\.")[0];

            // 🚨 PLATFORM IS NOT A TENANT
            if (
                    subdomain.equalsIgnoreCase("platform") ||
                            subdomain.equalsIgnoreCase("www") ||
                            subdomain.equalsIgnoreCase("localhost")
            ) {
                return null;
            }

            return subdomain;
        }

        // 3️⃣ Path fallback
        String path = request.getRequestURI();

        if (path.startsWith("/t/")) {

            String[] parts = path.split("/");

            if (parts.length > 2) {
                return parts[2];
            }
        }

        return null;
    }
}