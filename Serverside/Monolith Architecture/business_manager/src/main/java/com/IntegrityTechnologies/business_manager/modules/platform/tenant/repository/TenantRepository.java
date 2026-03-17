package com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.TenantStatus;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface TenantRepository extends JpaRepository<Tenant, UUID> {

    Optional<Tenant> findByCodeIgnoreCase(String code);

    boolean existsByCode(String code);

    Page<Tenant> findByStatus(
            TenantStatus status,
            Pageable pageable
    );
}