package com.IntegrityTechnologies.business_manager.modules.person.user.repository;

import com.IntegrityTechnologies.business_manager.modules.person.user.model.UserAudit;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.UUID;

public interface UserAuditRepository extends JpaRepository<UserAudit, UUID> {

    @Query("""
    SELECT ua
    FROM UserAudit ua
    WHERE ua.tenantId = :tenantId
      AND ua.userId = :userId
    ORDER BY ua.timestamp DESC
""")
    Page<UserAudit> findByUserIdOrderByTimestampDesc(
            @Param("tenantId") UUID tenantId,
            @Param("userId") UUID userId,
            Pageable pageable
    );

    @Query("""
        SELECT ua
        FROM UserAudit ua
        WHERE ua.tenantId = :tenantId
          AND ua.branchId = :branchId
          AND ua.performedById = :performedById
        ORDER BY ua.timestamp DESC
    """)
    List<UserAudit> findByPerformedByIdOrderByTimestampDesc(
            @Param("tenantId") UUID tenantId,
            @Param("performedById") UUID performedById
    );

    @Query("""
        SELECT ua
        FROM UserAudit ua
        WHERE ua.tenantId = :tenantId
        ORDER BY ua.timestamp DESC
    """)
    List<UserAudit> findTop5ByOrderByTimestampDesc(
            @Param("tenantId") UUID tenantId,
            Pageable pageable
    );

    List<UserAudit> findTop10ByTenantIdOrderByTimestampDesc(UUID tenantId);
}