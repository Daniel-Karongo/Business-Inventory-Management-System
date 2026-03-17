package com.IntegrityTechnologies.business_manager.security;

import jakarta.persistence.EntityManager;
import lombok.RequiredArgsConstructor;
import org.hibernate.Filter;
import org.hibernate.Session;
import org.springframework.stereotype.Component;

import java.util.function.Supplier;

@Component
@RequiredArgsConstructor
public class BranchFilterDisabler {

    private final EntityManager entityManager;

    public <T> T runWithoutBranchFilter(Supplier<T> action) {

        Session session = entityManager.unwrap(Session.class);

        Filter filter = session.getEnabledFilter("branchFilter");

        boolean wasEnabled = filter != null;

        try {

            if (wasEnabled) {
                session.disableFilter("branchFilter");
            }

            return action.get();

        } finally {

            if (wasEnabled) {
                session.enableFilter("branchFilter");
            }

        }
    }

    public void runWithoutBranchFilter(Runnable action) {

        runWithoutBranchFilter(() -> {
            action.run();
            return null;
        });

    }
}