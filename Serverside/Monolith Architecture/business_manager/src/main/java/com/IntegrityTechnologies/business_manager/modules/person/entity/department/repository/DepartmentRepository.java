package com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.*;

public interface DepartmentRepository extends JpaRepository<Department, UUID> {

    /* =========================================================
       SAFE FETCH
    ========================================================= */

    Optional<Department> findByTenantIdAndId(UUID tenantId, UUID id);

    Optional<Department> findByTenantIdAndIdAndDeletedFalse(UUID tenantId, UUID id);

    Optional<Department> findByTenantIdAndIdAndDeletedTrue(UUID tenantId, UUID id);

    boolean existsByTenantIdAndNameIgnoreCaseAndBranch_Id(
            UUID tenantId,
            String name,
            UUID branchId
    );

    Optional<Department> findByTenantIdAndNameIgnoreCaseAndBranch_Id(
            UUID tenantId,
            String name,
            UUID branchId
    );

    /* =========================================================
       LISTING
    ========================================================= */

    @EntityGraph(attributePaths = "branch")
    List<Department> findByTenantIdAndDeletedFalse(UUID tenantId);

    List<Department> findByTenantIdAndDeletedTrue(UUID tenantId);

    List<Department> findByTenantIdAndBranch_Id(UUID tenantId, UUID branchId);

    /* =========================================================
       USERS
    ========================================================= */

    @Query("""
        SELECT DISTINCT u
        FROM UserDepartment ud
        JOIN ud.user u
        LEFT JOIN FETCH u.emailAddresses
        LEFT JOIN FETCH u.phoneNumbers
        WHERE ud.department.id = :deptId
          AND ud.department.tenantId = :tenantId
          AND ud.department.deleted = false
          AND u.deleted = false
    """)
    List<User> findAllUsersInDepartment(
            @Param("tenantId") UUID tenantId,
            @Param("deptId") UUID deptId
    );

    @Query("""
        SELECT DISTINCT ud.department
        FROM UserDepartment ud
        WHERE ud.user.id = :userId
          AND ud.department.tenantId = :tenantId
          AND ud.department.deleted = false
    """)
    Set<Department> findDepartmentsByUserId(
            @Param("tenantId") UUID tenantId,
            @Param("userId") UUID userId
    );

    @Query("""
        SELECT d
        FROM UserDepartment ud
        JOIN ud.department d
        WHERE ud.user.id = :userId
          AND d.branch.id = :branchId
          AND d.tenantId = :tenantId
          AND d.deleted = false
    """)
    List<Department> findDepartmentsForUserInBranch(
            @Param("tenantId") UUID tenantId,
            @Param("userId") UUID userId,
            @Param("branchId") UUID branchId
    );

/* =========================================================
   ACTIVE
========================================================= */

    @Query("""
        SELECT d
        FROM Department d
        JOIN FETCH d.branch
        WHERE d.tenantId = :tenantId
          AND d.deleted = false
    """)
    List<Department> findAllActiveWithBranch(
            @Param("tenantId") UUID tenantId
    );
}