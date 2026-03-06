package com.IntegrityTechnologies.business_manager.modules.platform.tenant.config;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.context.TenantContext;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.servlet.*;
import org.hibernate.Session;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.UUID;

@Component
public class TenantHibernateFilterConfigurer implements Filter {

    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public void doFilter(
            ServletRequest request,
            ServletResponse response,
            FilterChain chain
    ) throws IOException, ServletException {

        Session session = entityManager.unwrap(Session.class);

        try {

            UUID tenantId = TenantContext.getOrNull();

            if (tenantId != null) {

                org.hibernate.Filter tenantFilter =
                        session.enableFilter("tenantFilter");

                tenantFilter.setParameter("tenantId", tenantId);

            }

            chain.doFilter(request, response);

        } finally {

            if (session.getEnabledFilter("tenantFilter") != null) {
                session.disableFilter("tenantFilter");
            }

        }
    }
}