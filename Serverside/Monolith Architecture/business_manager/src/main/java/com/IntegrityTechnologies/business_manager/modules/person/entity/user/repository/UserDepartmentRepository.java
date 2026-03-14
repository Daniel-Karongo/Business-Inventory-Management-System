package com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.UserDepartment;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.UserDepartmentId;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.UUID;

public interface UserDepartmentRepository
        extends JpaRepository<UserDepartment, UserDepartmentId> {

    boolean existsByUser_IdAndDepartment_Id(UUID userId, UUID departmentId);

    @Modifying
    @Query("delete from UserDepartment ud where ud.user.id = :userId")
    void deleteByUserId(UUID userId);

    List<UserDepartment> findByUserId(UUID userId);

    List<UserDepartment> findByDepartmentId(UUID departmentId);

    @Query("""
        SELECT ud
        FROM UserDepartment ud
        JOIN FETCH ud.user
        WHERE ud.department.id = :departmentId
          AND ud.department.tenantId = :tenantId
    """)
    List<UserDepartment> findByDepartmentIdWithUser(
            @Param("tenantId") UUID tenantId,
            @Param("departmentId") UUID departmentId
    );

    @Query("""
        SELECT ud FROM UserDepartment ud
        JOIN FETCH ud.department d
        JOIN FETCH d.branch
        WHERE ud.user.id = :userId
    """)
    List<UserDepartment> findByUserIdWithDepartmentAndBranch(UUID userId);

    @Query("""
        SELECT ud
        FROM UserDepartment ud
        WHERE ud.department.id = :departmentId
          AND ud.department.tenantId = :tenantId
    """)
    Page<UserDepartment> findByDepartmentId(
            @Param("tenantId") UUID tenantId,
            @Param("departmentId") UUID departmentId,
            Pageable pageable
    );

    @Modifying
    @Query("DELETE FROM UserDepartment ud WHERE ud.department.id = :departmentId")
    void deleteByDepartmentId(UUID departmentId);
}