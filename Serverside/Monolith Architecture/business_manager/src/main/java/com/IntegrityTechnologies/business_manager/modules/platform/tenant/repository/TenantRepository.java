package com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface TenantRepository extends JpaRepository<Tenant, UUID> {

    @Cacheable(value = "tenants", key = "#code")
    Optional<Tenant> findByCode(String code);

    boolean existsByCode(String code);

}