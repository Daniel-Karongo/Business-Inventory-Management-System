package com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository;

import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.TenantSubscription;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.Optional;
import java.util.UUID;

public interface TenantSubscriptionRepository extends JpaRepository<TenantSubscription, UUID> {
    @Query("""
        SELECT s
        FROM TenantSubscription s
        JOIN FETCH s.plan
        WHERE s.tenantId = :tenantId
    """)
    Optional<TenantSubscription> findByTenantId(UUID tenantId);

    boolean existsByTenantId(UUID tenantId);
}