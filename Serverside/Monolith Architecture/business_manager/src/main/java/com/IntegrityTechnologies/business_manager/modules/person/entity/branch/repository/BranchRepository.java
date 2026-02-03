package com.IntegrityTechnologies.business_manager.modules.person.entity.branch.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.entity.branch.model.Branch;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
@Repository
public interface BranchRepository extends JpaRepository<Branch, UUID> {

    Optional<Branch> findByBranchCode(String branchCode);
    Optional<Branch> findByIdAndDeletedFalse(UUID id);
    Optional<Branch> findByIdAndDeletedTrue(UUID id);
    @Query("SELECT u FROM Branch b JOIN b.users u WHERE b.id = :branchId")
    List<User> findUsersByBranchId(@Param("branchId") UUID branchId);
    @Query("""
       SELECT b 
       FROM Branch b 
       JOIN b.users u 
       WHERE u.id = :userId
       """)
    List<Branch> findBranchesByUserId(@Param("userId") UUID userId);

    @Query("""
        SELECT DISTINCT b
        FROM Branch b
        JOIN b.users u
        JOIN b.departments d
        WHERE u.id = :userId
          AND d.id = :deptId
    """)
    List<Branch> findBranchesForUserAndDepartment(@Param("userId") UUID userId,
                                                  @Param("deptId") UUID deptId);


    @Query("""
    SELECT CASE WHEN COUNT(b) > 0 THEN TRUE ELSE FALSE END
    FROM Branch b
    JOIN b.departments d
    WHERE b.id = :branchId AND d.id = :departmentId
""")
    boolean branchContainsDepartment(@Param("branchId") UUID branchId, @Param("departmentId") UUID departmentId);

    boolean existsByBranchCodeIgnoreCase(String branchCode);
}