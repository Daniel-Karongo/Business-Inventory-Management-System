package com.IntegrityTechnologies.business_manager.security.filter;

import com.IntegrityTechnologies.business_manager.security.cache.TenantMetadataCache;
import com.IntegrityTechnologies.business_manager.security.util.TenantContext;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.Session;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.UUID;

@Slf4j
@Component
@RequiredArgsConstructor
public class TenantResolutionFilter extends OncePerRequestFilter {

    private final TenantMetadataCache tenantMetadataCache;

    @PersistenceContext
    private EntityManager entityManager;

    private static final String TENANT_HEADER = "X-Tenant";

    @Override
    protected void doFilterInternal(
            HttpServletRequest request,
            HttpServletResponse response,
            FilterChain filterChain
    ) throws ServletException, IOException {

        String tenantCode = resolveTenant(request);

        if (tenantCode == null) {
            filterChain.doFilter(request, response);
            return;
        }

        try {

            Session session = entityManager.unwrap(Session.class);

            if (session.getEnabledFilter("tenantFilter") != null) {
                session.disableFilter("tenantFilter");
            }

            UUID tenantId =
                    tenantMetadataCache.getTenantIdByCode(tenantCode);

            TenantContext.setTenantId(tenantId);

            filterChain.doFilter(request, response);

        } finally {
            // Safety net (final cleanup filter also handles it)
            TenantContext.clear();
        }
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