package com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.*;

public interface DepartmentRepository extends JpaRepository<Department, UUID> {

    Optional<Department> findByIdAndDeletedFalse(UUID id);

    Optional<Department> findByIdAndDeletedTrue(UUID id);

    boolean existsByNameIgnoreCaseAndBranch_Id(String name, UUID branchId);

    List<Department> findByDeletedFalse();

    List<Department> findByDeletedTrue();

    List<Department> findByBranch_Id(UUID branchId);

    Optional<Department> findByNameIgnoreCaseAndBranch_Id(String name, UUID branchId);

    /* ✅ ADD THIS METHOD (Fix error) */
    Optional<Department> findByNameIgnoreCase(String name);


    @Query("""
        SELECT d
        FROM Department d
        WHERE d.deleted = false
    """)
    List<Department> findAllActive();


    @Query("""
        SELECT DISTINCT ud.user
        FROM UserDepartment ud
        WHERE ud.department.id = :deptId
          AND ud.department.deleted = false
          AND ud.user.deleted = false
    """)
    List<User> findAllUsersInDepartment(@Param("deptId") UUID deptId);


    @Query("""
        SELECT DISTINCT ud.department
        FROM UserDepartment ud
        WHERE ud.user.id = :userId
          AND ud.department.deleted = false
    """)
    Set<Department> findDepartmentsByUserId(@Param("userId") UUID userId);

    @Query("""
    SELECT d
    FROM UserDepartment ud
    JOIN ud.department d
    WHERE ud.user.id = :userId
      AND d.branch.id = :branchId
      AND d.deleted = false
""")
    List<Department> findDepartmentsForUserInBranch(
            @Param("userId") UUID userId,
            @Param("branchId") UUID branchId
    );
}