package com.IntegrityTechnologies.business_manager.modules.person.function.department.repository;

import com.IntegrityTechnologies.business_manager.modules.person.function.department.model.Department;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
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

    // find members by department id
    // --- Find members by department id ---
    @Query("SELECT m FROM Department d JOIN d.members m " +
            "WHERE d.id = :deptId AND m.deleted = false")
    List<User> findMembersByDepartmentId(@Param("deptId") UUID deptId);

    // --- Find heads by department id ---
    @Query("SELECT h FROM Department d JOIN d.heads h " +
            "WHERE d.id = :deptId AND h.deleted = false")
    List<User> findHeadsByDepartmentId(@Param("deptId") UUID deptId);

    @Query("""
    SELECT DISTINCT u
    FROM Department d
    LEFT JOIN d.members m
    LEFT JOIN d.heads h
    LEFT JOIN User u ON u.id = m.id OR u.id = h.id
    WHERE d.id = :deptId
      AND d.deleted = false
      AND u.deleted = false
    """)
    List<User> findAllUsersInDepartment(@Param("deptId") UUID deptId);

    @Query("""
    SELECT DISTINCT d 
    FROM Department d
    LEFT JOIN d.heads h
    LEFT JOIN d.members m
    WHERE d.deleted = false
      AND (
            (h IS NOT NULL AND h.id = :userId AND h.deleted = false)
         OR (m IS NOT NULL AND m.id = :userId AND m.deleted = false)
      )
    """)
    List<Department> findDepartmentsByUserId(@Param("userId") UUID userId);

}