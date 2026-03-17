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

    /* ================= DELETE ================= */

    @Modifying
    @Query("""
        DELETE FROM UserDepartment ud
        WHERE ud.user.id = :userId
          AND ud.user.tenantId = :tenantId
    """)
    void deleteByUserId(
            @Param("tenantId") UUID tenantId,
            @Param("userId") UUID userId
    );

    /* ================= TENANT SAFE ================= */

    @Query("""
        SELECT ud
        FROM UserDepartment ud
        WHERE ud.user.id = :userId
          AND ud.user.tenantId = :tenantId
    """)
    List<UserDepartment> findByUserId(
            @Param("tenantId") UUID tenantId,
            @Param("userId") UUID userId
    );

    @Query("""
        SELECT ud
        FROM UserDepartment ud
        WHERE ud.department.id = :departmentId
          AND ud.department.tenantId = :tenantId
    """)
    List<UserDepartment> findByDepartmentId(
            @Param("tenantId") UUID tenantId,
            @Param("departmentId") UUID departmentId
    );

    /* ================= FETCH ================= */

    @Query("""
    SELECT ud
    FROM UserDepartment ud
    JOIN FETCH ud.user u
    WHERE ud.department.id = :departmentId
      AND ud.department.tenantId = :tenantId
""")
    List<UserDepartment> findByDepartmentIdWithUser(
            @Param("tenantId") UUID tenantId,
            @Param("departmentId") UUID departmentId
    );

    @Query("""
        SELECT ud
        FROM UserDepartment ud
        JOIN FETCH ud.department d
        JOIN FETCH d.branch
        WHERE ud.user.id = :userId
          AND d.tenantId = :tenantId
    """)
    List<UserDepartment> findByUserIdWithDepartmentAndBranch(
            @Param("tenantId") UUID tenantId,
            @Param("userId") UUID userId
    );

    /* ================= PAGINATION ================= */

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

    /* ================= DELETE ================= */

    @Modifying
    @Query("""
        DELETE FROM UserDepartment ud
        WHERE ud.department.id = :departmentId
          AND ud.department.tenantId = :tenantId
    """)
    void deleteByDepartmentId(
            @Param("tenantId") UUID tenantId,
            @Param("departmentId") UUID departmentId
    );
}