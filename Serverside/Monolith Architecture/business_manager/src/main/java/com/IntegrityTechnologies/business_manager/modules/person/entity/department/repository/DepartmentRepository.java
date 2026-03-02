package com.IntegrityTechnologies.business_manager.modules.person.entity.department.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import com.IntegrityTechnologies.business_manager.modules.person.entity.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

@Repository
public interface DepartmentRepository extends JpaRepository<Department, UUID> {
    Optional<Department> findByNameIgnoreCase(String name);
    Optional<Department> findByIdAndDeletedFalse(UUID id);
    Optional<Department> findByIdAndDeletedTrue(UUID id);
    Optional<Department> findById(UUID id);

    @Query("SELECT d FROM Department d WHERE d.deleted = false")
    List<Department> findAllActive();
    List<Department> findByDeletedFalse();
    List<Department> findByDeletedTrue();

    Boolean existsByNameIgnoreCase(String name);

    @Query("""
        SELECT DISTINCT ud.user
        FROM UserDepartment ud
        JOIN ud.department d
        WHERE d.id = :deptId
          AND d.deleted = false
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
        SELECT DISTINCT ud.department
        FROM UserDepartment ud
        WHERE ud.user.id = :userId
          AND ud.department.branch.id = :branchId
          AND ud.department.deleted = false
        """)
    List<Department> findDepartmentsForUserInBranch(
            @Param("userId") UUID userId,
            @Param("branchId") UUID branchId);

    @Query("""
    SELECT b
    FROM Branch b
    JOIN b.departments d
    WHERE d.id = :deptId
      AND b.deleted = false
""")
    List<Branch> findBranchesByDepartmentId(@Param("deptId") UUID deptId);

    Optional<Department> findByBranchId(UUID id);

    boolean existsByNameIgnoreCaseAndBranch_Id(String name, UUID branchId);

    List<Department> findByBranch_Id(UUID branchId);
}