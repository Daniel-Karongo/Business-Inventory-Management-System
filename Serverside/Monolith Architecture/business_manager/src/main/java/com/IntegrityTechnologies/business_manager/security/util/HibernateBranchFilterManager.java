package com.IntegrityTechnologies.business_manager.security.util;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import org.hibernate.Session;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class HibernateBranchFilterManager {

    @PersistenceContext
    private EntityManager entityManager;

    public void enable(UUID branchId) {

        Session session = entityManager.unwrap(Session.class);

        if (session.getEnabledFilter("branchFilter") != null) {
            session.disableFilter("branchFilter");
        }

        // 🔥 KEY RULE
        if (branchId == null) {
            return; // NO FILTER → GLOBAL ACCESS
        }

        session.enableFilter("branchFilter")
                .setParameter("branchId", branchId);
    }
}