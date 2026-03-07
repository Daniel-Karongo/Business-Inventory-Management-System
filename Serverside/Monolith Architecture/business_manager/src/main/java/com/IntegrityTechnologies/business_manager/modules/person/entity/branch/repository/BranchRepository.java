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

    /* ================= BASIC LOOKUPS ================= */

    List<Branch> findByDeleted(Boolean deleted);

    List<Branch> findByDeletedFalse();

    Page<Branch> findByDeletedFalse(Pageable pageable);

    Optional<Branch> findByBranchCodeIgnoreCase(String branchCode);

    /* ✅ ADD THIS METHOD (Fix error) */
    Optional<Branch> findByBranchCode(String branchCode);

    Optional<Branch> findByIdAndDeletedFalse(UUID id);

    Optional<Branch> findByIdAndDeletedTrue(UUID id);

    boolean existsByBranchCodeIgnoreCase(String branchCode);

    int countByTenantId(UUID tenantId);


    /* ================= RELATION LOOKUPS ================= */

    @Query("""
        SELECT ub.user
        FROM UserBranch ub
        WHERE ub.branch.id = :branchId
          AND ub.user.deleted = false
    """)
    List<User> findUsersByBranchId(@Param("branchId") UUID branchId);


    @Query("""
        SELECT DISTINCT ub.branch
        FROM UserBranch ub
        JOIN FETCH ub.branch b
        WHERE ub.user.id = :userId
          AND b.deleted = false
    """)
    List<Branch> findBranchesByUserId(UUID userId);


    @Query("""
        SELECT DISTINCT ub.branch
        FROM UserBranch ub
        JOIN UserDepartment ud ON ud.user.id = ub.user.id
        WHERE ub.user.id = :userId
          AND ud.department.id = :deptId
          AND ub.branch.id = ud.department.branch.id
          AND ub.branch.deleted = false
          AND ud.department.deleted = false
    """)
    List<Branch> findBranchesForUserAndDepartment(
            @Param("userId") UUID userId,
            @Param("deptId") UUID deptId
    );


    @Query("""
        SELECT CASE WHEN COUNT(d) > 0 THEN TRUE ELSE FALSE END
        FROM Department d
        WHERE d.branch.id = :branchId
          AND d.id = :departmentId
          AND d.deleted = false
    """)
    boolean branchContainsDepartment(
            @Param("branchId") UUID branchId,
            @Param("departmentId") UUID departmentId
    );


    /* ================= BULK ================= */

    @Modifying
    @Query("""
        UPDATE Branch b
        SET b.deleted = true
        WHERE b.id IN :ids
    """)
    void softDeleteBulk(@Param("ids") List<UUID> ids);

    boolean existsByIdAndTenantId(UUID branchId, UUID tenantId);
}