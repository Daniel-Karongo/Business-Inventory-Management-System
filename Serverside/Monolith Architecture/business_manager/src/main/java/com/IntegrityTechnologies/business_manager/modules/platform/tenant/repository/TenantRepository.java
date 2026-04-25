package com.IntegrityTechnologies.business_manager.modules.platform.tenant.repository;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.Tenant;
import com.IntegrityTechnologies.business_manager.modules.platform.tenant.entity.TenantStatus;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

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

    List<Tenant> findByStatusIn(List<TenantStatus> statuses);

    Page<Tenant> findByPlatformTenantFalse(
            Pageable pageable
    );

    @Query("""
            SELECT t
            FROM Tenant t
            WHERE t.platformTenant = false
            AND (
                :search IS NULL
                OR :search = ''
                OR lower(t.name) like lower(concat('%', :search, '%'))
                OR lower(t.code) like lower(concat('%', :search, '%'))
            )
            """)
    Page<Tenant> searchTenants(
            String search,
            Pageable pageable
    );
}