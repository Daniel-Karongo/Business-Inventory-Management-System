package com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.UserBranch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.UserBranchId;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.UUID;

public interface UserBranchRepository
        extends JpaRepository<UserBranch, UserBranchId> {

    boolean existsByUser_IdAndBranch_Id(UUID userId, UUID branchId);

    /* ================= TENANT SAFE ================= */

    @Query("""
        SELECT ub
        FROM UserBranch ub
        WHERE ub.branch.tenantId = :tenantId
          AND ub.branch.id = :branchId
    """)
    List<UserBranch> findByBranchId(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    @Query("""
        SELECT ub
        FROM UserBranch ub
        WHERE ub.user.tenantId = :tenantId
          AND ub.user.id = :userId
    """)
    List<UserBranch> findByUserId(
            @Param("tenantId") UUID tenantId,
            @Param("userId") UUID userId
    );

    /* ================= DELETE ================= */

    @Modifying
    @Query("""
        DELETE FROM UserBranch ub
        WHERE ub.user.id = :userId
          AND ub.user.tenantId = :tenantId
    """)
    void deleteByUserId(
            @Param("tenantId") UUID tenantId,
            @Param("userId") UUID userId
    );

    @Modifying
    @Query("""
        DELETE FROM UserBranch ub
        WHERE ub.branch.id = :branchId
          AND ub.branch.tenantId = :tenantId
    """)
    void deleteByBranchId(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );
}