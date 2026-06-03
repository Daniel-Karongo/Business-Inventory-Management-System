package com.IntegrityTechnologies.business_manager.modules.person.branch.repository;

import com.IntegrityTechnologies.business_manager.modules.person.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.User;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface BranchRepository extends JpaRepository<Branch, UUID>,
        JpaSpecificationExecutor<Branch> {

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

    Optional<Branch> findByTenantIdAndBranchCodeIgnoreCaseAndDeletedFalse(UUID tenantId, String branchCode);

    boolean existsByTenantIdAndBranchCodeIgnoreCase(UUID tenantId, String branchCode);

    /* =========================================================
       LISTING
    ========================================================= */

    List<Branch> findByTenantIdAndDeletedFalse(UUID tenantId);

    Page<Branch> findByTenantIdAndDeletedFalse(UUID tenantId, Pageable pageable);

    List<Branch> findByTenantIdAndDeletedTrue(UUID tenantId);

    Page<Branch> findAll(
            Specification<Branch> spec,
            Pageable pageable
    );

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
                SELECT CASE WHEN COUNT(ub) > 0 THEN TRUE ELSE FALSE END
                FROM UserBranch ub
                WHERE ub.user.id = :userId
                AND ub.branch.id = :branchId
                AND ub.branch.tenantId = :tenantId
                AND ub.branch.deleted = false
            """)
    boolean userBelongsToBranch(
            @Param("tenantId") UUID tenantId,
            @Param("userId") UUID userId,
            @Param("branchId") UUID branchId
    );
}