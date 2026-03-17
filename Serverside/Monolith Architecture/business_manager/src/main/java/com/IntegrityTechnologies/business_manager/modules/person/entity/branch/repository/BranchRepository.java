package com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface BranchRepository extends JpaRepository<Branch, UUID> {

    /* =========================================================
       SAFE FETCH
    ========================================================= */

    Optional<Branch> findByTenantIdAndId(UUID tenantId, UUID id);

    Optional<Branch> findByTenantIdAndIdAndDeletedFalse(UUID tenantId, UUID id);

    Optional<Branch> findByTenantIdAndIdAndDeletedTrue(UUID tenantId, UUID id);

    boolean existsByTenantIdAndId(UUID tenantId, UUID branchId);

    int countByTenantId(UUID tenantId);

    /* =========================================================
       BRANCH CODE
    ========================================================= */

    Optional<Branch> findByTenantIdAndBranchCodeIgnoreCase(UUID tenantId, String branchCode);

    boolean existsByTenantIdAndBranchCodeIgnoreCase(UUID tenantId, String branchCode);

    /* =========================================================
       LISTING
    ========================================================= */

    List<Branch> findByTenantIdAndDeletedFalse(UUID tenantId);

    List<Branch> findByTenantIdAndDeletedTrue(UUID tenantId);

    Page<Branch> findByTenantIdAndDeletedFalse(UUID tenantId, Pageable pageable);

    /* =========================================================
       RELATIONS
    ========================================================= */

    @Query("""
    SELECT DISTINCT u
    FROM UserBranch ub
    JOIN ub.user u
    LEFT JOIN FETCH u.emailAddresses
    LEFT JOIN FETCH u.phoneNumbers
    WHERE ub.branch.id = :branchId
      AND ub.branch.tenantId = :tenantId
      AND u.deleted = false
""")
    List<User> findUsersByBranchId(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId
    );

    @Query("""
        SELECT DISTINCT ub.branch
        FROM UserBranch ub
        WHERE ub.user.id = :userId
          AND ub.branch.tenantId = :tenantId
          AND ub.branch.deleted = false
    """)
    List<Branch> findBranchesByUserId(
            @Param("tenantId") UUID tenantId,
            @Param("userId") UUID userId
    );

    @Query("""
        SELECT DISTINCT ub.branch
        FROM UserBranch ub
        JOIN UserDepartment ud ON ud.user.id = ub.user.id
        WHERE ub.user.id = :userId
          AND ud.department.id = :deptId
          AND ub.branch.id = ud.department.branch.id
          AND ub.branch.tenantId = :tenantId
          AND ub.branch.deleted = false
          AND ud.department.deleted = false
    """)
    List<Branch> findBranchesForUserAndDepartment(
            @Param("tenantId") UUID tenantId,
            @Param("userId") UUID userId,
            @Param("deptId") UUID deptId
    );

    @Query("""
        SELECT CASE WHEN COUNT(d) > 0 THEN TRUE ELSE FALSE END
        FROM Department d
        WHERE d.branch.id = :branchId
          AND d.branch.tenantId = :tenantId
          AND d.id = :departmentId
          AND d.deleted = false
    """)
    boolean branchContainsDepartment(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId,
            @Param("departmentId") UUID departmentId
    );

    /* =========================================================
       BULK
    ========================================================= */

    @Modifying
    @Query("""
        UPDATE Branch b
        SET b.deleted = true
        WHERE b.tenantId = :tenantId
          AND b.id IN :ids
    """)
    void softDeleteBulk(
            @Param("tenantId") UUID tenantId,
            @Param("ids") List<UUID> ids
    );
}