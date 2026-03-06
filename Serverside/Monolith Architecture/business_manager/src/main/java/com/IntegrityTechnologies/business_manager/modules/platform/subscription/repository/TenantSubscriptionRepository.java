package com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository;

import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.TenantSubscription;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface TenantSubscriptionRepository extends JpaRepository<TenantSubscription, UUID> {

    Optional<TenantSubscription> findByTenantId(UUID tenantId);

}