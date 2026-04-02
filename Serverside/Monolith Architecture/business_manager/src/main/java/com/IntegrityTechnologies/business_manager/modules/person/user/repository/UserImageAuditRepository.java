package com.IntegrityTechnologies.business_manager.modules.person.user.repository;

import com.IntegrityTechnologies.business_manager.modules.person.user.model.UserImageAudit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.UUID;

public interface UserImageAuditRepository extends JpaRepository<UserImageAudit, UUID> {

    @Query("""
        SELECT ua
        FROM UserImageAudit ua
        WHERE ua.tenantId = :tenantId
          AND ua.branchId = :branchId
          AND ua.userId = :userId
        ORDER BY ua.timestamp DESC
    """)
    List<UserImageAudit> findByUserIdOrderByTimestampDesc(
            @Param("tenantId") UUID tenantId,
            @Param("userId") UUID userId
    );

    @Query("""
        SELECT ua
        FROM UserImageAudit ua
        WHERE ua.tenantId = :tenantId
          AND ua.branchId = :branchId
          AND ua.performedById = :performedById
        ORDER BY ua.timestamp DESC
    """)
    List<UserImageAudit> findByPerformedByIdOrderByTimestampDesc(
            @Param("tenantId") UUID tenantId,
            @Param("performedById") UUID performedById
    );
}