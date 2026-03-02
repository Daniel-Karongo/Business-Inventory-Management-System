package com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
@Repository
public interface BranchRepository extends JpaRepository<Branch, UUID> {

    List<Branch> findByDeleted(Boolean deleted);
    List<Branch> findByDeletedFalse();
    Optional<Branch> findByBranchCode(String branchCode);
    Optional<Branch> findByIdAndDeletedFalse(UUID id);
    Optional<Branch> findByIdAndDeletedTrue(UUID id);

    @Query("""
        SELECT ub.user
        FROM UserBranch ub
        WHERE ub.branch.id = :branchId
          AND ub.user.deleted = false
    """)
    List<User> findUsersByBranchId(@Param("branchId") UUID branchId);
    @Query("""
        SELECT ub.branch
        FROM UserBranch ub
        WHERE ub.user.id = :userId
          AND ub.branch.deleted = false
    """)
    List<Branch> findBranchesByUserId(@Param("userId") UUID userId);
    @Query("""
        SELECT DISTINCT ud.department.branch
        FROM UserDepartment ud
        WHERE ud.user.id = :userId
          AND ud.department.id = :deptId
          AND ud.department.deleted = false
    """)
    List<Branch> findBranchesForUserAndDepartment(
            @Param("userId") UUID userId,
            @Param("deptId") UUID deptId);
    @Query("""
        SELECT CASE WHEN COUNT(d) > 0 THEN TRUE ELSE FALSE END
        FROM Department d
        WHERE d.branch.id = :branchId
          AND d.id = :departmentId
          AND d.deleted = false
    """)
    boolean branchContainsDepartment(
            @Param("branchId") UUID branchId,
            @Param("departmentId") UUID departmentId);

    boolean existsByBranchCodeIgnoreCase(String branchCode);
    @Modifying
    @Query("UPDATE Branch b SET b.deleted = true WHERE b.id IN :ids")
    void softDeleteBulk(@Param("ids") List<UUID> ids);
}