package com.IntegrityTechnologies.business_manager.modules.platform.subscription.repository;

import com.IntegrityTechnologies.business_manager.modules.platform.subscription.entity.Plan;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.Optional;
import java.util.UUID;

public interface PlanRepository extends JpaRepository<Plan, UUID> {

    Optional<Plan> findByCode(String code);
    @Query("""
        SELECT s.plan
        FROM TenantSubscription s
        WHERE s.tenantId = :tenantId
    """)
    Optional<Plan> findPlanByTenantId(UUID tenantId);
}