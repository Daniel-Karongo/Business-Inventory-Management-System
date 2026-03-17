package com.IntegrityTechnologies.business_manager.security;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import lombok.RequiredArgsConstructor;
import org.hibernate.Session;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
@RequiredArgsConstructor
public class HibernateTenantFilterManager {

    @PersistenceContext
    private EntityManager entityManager;

    public void enable(UUID tenantId) {

        if (tenantId == null) return;

        Session session = entityManager.unwrap(Session.class);

        // ✅ Prevent duplicate / stale filters
        if (session.getEnabledFilter("tenantFilter") != null) {
            session.disableFilter("tenantFilter");
        }

        session.enableFilter("tenantFilter")
                .setParameter("tenantId", tenantId);
    }
}