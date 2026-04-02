package com.IntegrityTechnologies.business_manager.modules.person.user.repository;

import com.IntegrityTechnologies.business_manager.modules.person.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.User;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface UserRepository extends JpaRepository<User, UUID> {

    int countByTenantId(UUID tenantId);

    /* ====================== BASIC LOOKUPS ====================== */
    @Query("""
        SELECT u
        FROM User u
        WHERE u.tenantId = :tenantId
    """)
    List<User> findByTenantId(
            @Param("tenantId") UUID tenantId
    );

    @Query("""
        SELECT u
        FROM User u
        WHERE u.tenantId = :tenantId
    """)
    Page<User> findAllUsers(UUID tenantId, Pageable pageable);


    @Query("""
        SELECT u
        FROM User u
        WHERE u.tenantId = :tenantId
        AND u.deleted = false
    """)
    Page<User> findActiveUsers(UUID tenantId, Pageable pageable);


    @Query("""
        SELECT u
        FROM User u
        WHERE u.tenantId = :tenantId
        AND u.deleted = true
    """)
    Page<User> findDeletedUsers(UUID tenantId, Pageable pageable);

    @Query("""
    SELECT DISTINCT u.id
    FROM User u
    LEFT JOIN u.departments ud
    LEFT JOIN ud.department d
    LEFT JOIN d.branch b
    WHERE u.tenantId = :tenantId
    AND (:deleted IS NULL OR u.deleted = :deleted)
    AND (:role IS NULL OR u.role = :role)
    AND (:branchId IS NULL OR b.id = :branchId)
    AND (:departmentId IS NULL OR d.id = :departmentId)
    AND (
        :q IS NULL OR
        LOWER(u.username) LIKE LOWER(CONCAT('%', :q, '%'))
        OR EXISTS (
            SELECT 1 FROM u.emailAddresses e
            WHERE e LIKE CONCAT('%', :q, '%')
        )
        OR EXISTS (
            SELECT 1 FROM u.phoneNumbers p
            WHERE p LIKE CONCAT('%', :q, '%')
        )
    )
""")
    Page<UUID> searchUserIds(
            @Param("tenantId") UUID tenantId,
            @Param("deleted") Boolean deleted,
            @Param("role") Role role,
            @Param("branchId") UUID branchId,
            @Param("departmentId") UUID departmentId,
            @Param("q") String q,
            Pageable pageable
    );

    @Query("""
        SELECT DISTINCT u
        FROM User u
        LEFT JOIN FETCH u.departments ud
        LEFT JOIN FETCH ud.department d
        LEFT JOIN FETCH d.branch
        WHERE u.id IN :ids
    """)
    List<User> findUsersWithDepartments(@Param("ids") List<UUID> ids);
    Optional<User> findByUsernameAndTenantId(String username, UUID tenantId);

    Optional<User> findByUsernameAndTenantIdAndDeletedFalse(String username, UUID tenantId);

    Optional<User> findByIdNumberAndTenantIdAndDeletedFalse(String idNumber, UUID tenantId);

    Optional<User> findByIdNumberAndTenantId(String idNumber, UUID tenantId);

    Optional<User> findByIdAndTenantIdAndDeletedFalse(UUID id, UUID tenantId);

    Optional<User> findByIdAndTenantId(UUID id, UUID tenantId);

    List<User> findByTenantIdAndDeletedFalse(UUID tenantId);

    List<User> findByTenantIdAndDeletedTrue(UUID tenantId);

    @Query("""
        SELECT DISTINCT u
        FROM User u
        LEFT JOIN FETCH u.departments ud
        LEFT JOIN FETCH ud.department d
        LEFT JOIN FETCH d.branch
        WHERE u.tenantId = :tenantId
    """)
    List<User> findAllUsersWithDepartments(
            @Param("tenantId") UUID tenantId
    );
    /* ====================== BULK ====================== */

    @Query("""
        SELECT u
        FROM User u
        WHERE u.id IN :ids
          AND u.tenantId = :tenantId
          AND u.deleted = false
    """)
    List<User> findAllByTenantIdAndIdIn(
            @Param("tenantId") UUID tenantId,
            @Param("ids") Collection<UUID> ids
    );

    /* ====================== UNIQUE CHECKS ====================== */

    boolean existsByUsernameAndTenantId(String username, UUID tenantId);

    boolean existsByIdNumberAndTenantId(String idNumber, UUID tenantId);

    @Query("""
        SELECT CASE WHEN COUNT(u) > 0 THEN true ELSE false END
        FROM User u
        JOIN u.emailAddresses e
        WHERE lower(e) = lower(:email)
          AND u.tenantId = :tenantId
    """)
    boolean existsByEmailAddressAndTenantId(
            @Param("email") String email,
            @Param("tenantId") UUID tenantId
    );

    @Query("""
        SELECT CASE WHEN COUNT(u) > 0 THEN true ELSE false END
        FROM User u
        JOIN u.phoneNumbers p
        WHERE p = :phone
          AND u.tenantId = :tenantId
    """)
    boolean existsByPhoneNumberAndTenantId(
            @Param("phone") String phone,
            @Param("tenantId") UUID tenantId
    );

    /* ====================== COLLECTION LOOKUP ====================== */

    @Query("""
        SELECT u FROM User u
        JOIN u.emailAddresses e
        WHERE lower(e) = lower(:email)
          AND u.deleted = false
          AND u.tenantId = :tenantId
    """)
    Optional<User> findByEmailElementIgnoreCaseAndDeletedFalse(
            @Param("email") String email,
            @Param("tenantId") UUID tenantId
    );

    @Query("""
        SELECT u FROM User u
        JOIN u.emailAddresses e
        WHERE lower(e) = lower(:email)
          AND u.tenantId = :tenantId
    """)
    Optional<User> findByEmailElementIgnoreCase(
            @Param("email") String email,
            @Param("tenantId") UUID tenantId
    );

    @Query("""
        SELECT u FROM User u
        JOIN u.phoneNumbers p
        WHERE p = :phone
          AND u.deleted = false
          AND u.tenantId = :tenantId
    """)
    Optional<User> findByPhoneNumberElementAndDeletedFalse(
            @Param("phone") String phone,
            @Param("tenantId") UUID tenantId
    );

    @Query("""
        SELECT u FROM User u
        JOIN u.phoneNumbers p
        WHERE p = :phone
          AND u.tenantId = :tenantId
    """)
    Optional<User> findByPhoneNumberElement(
            @Param("phone") String phone,
            @Param("tenantId") UUID tenantId
    );

    /* ====================== ROLE FILTER ====================== */

    @Query("""
    SELECT u
    FROM User u
    WHERE u.deleted = false
      AND u.role = :role
      AND u.tenantId = :tenantId
""")
    Page<User> findActiveUsersByRole(
            @Param("tenantId") UUID tenantId,
            @Param("role") Role role,
            Pageable pageable
    );

    @Query("""
    SELECT u
    FROM User u
    WHERE u.deleted = true
      AND u.role = :role
      AND u.tenantId = :tenantId
""")
    Page<User> findDeletedUsersByRole(
            @Param("tenantId") UUID tenantId,
            @Param("role") Role role,
            Pageable pageable
    );

    @Query("""
    SELECT u
    FROM User u
    WHERE u.role = :role
      AND u.tenantId = :tenantId
""")
    Page<User> findUsersByRole(
            @Param("tenantId") UUID tenantId,
            @Param("role") Role role,
            Pageable pageable
    );

    /* ====================== IDENTIFIER LOOKUP ====================== */

    @Query("""
        SELECT DISTINCT u
        FROM User u
        LEFT JOIN FETCH u.branches
        LEFT JOIN FETCH u.departments
        WHERE u.tenantId = :tenantId
          AND u.deleted = false
          AND (
                lower(u.username) = lower(:identifier)
             OR lower(u.idNumber) = lower(:identifier)
             OR EXISTS (
                    SELECT 1 FROM u.emailAddresses e
                    WHERE lower(e) = lower(:identifier)
                )
             OR EXISTS (
                    SELECT 1 FROM u.phoneNumbers p
                    WHERE p = :identifier
                )
          )
    """)
    Optional<User> findAuthUser(
            @Param("tenantId") UUID tenantId,
            @Param("identifier") String identifier
    );

    /* ====================== IDENTIFIER RESOLUTION ====================== */

    default Optional<User> findByIdentifier(String rawIdentifier, UUID tenantId) {

        String identifier = rawIdentifier == null ? "" : rawIdentifier.trim();

        try {
            UUID id = UUID.fromString(identifier);
            Optional<User> byId = findByIdAndTenantIdAndDeletedFalse(id, tenantId);
            if (byId.isPresent()) return byId;
        } catch (IllegalArgumentException ignored) {}

        Optional<User> maybe =
                findByEmailElementIgnoreCaseAndDeletedFalse(identifier.toLowerCase(), tenantId);
        if (maybe.isPresent()) return maybe;

        String phoneNumber = normalizePhone(identifier);

        maybe = findByPhoneNumberElementAndDeletedFalse(phoneNumber, tenantId);
        if (maybe.isPresent()) return maybe;

        maybe = findByUsernameAndTenantIdAndDeletedFalse(identifier, tenantId);
        if (maybe.isPresent()) return maybe;

        maybe = findByIdNumberAndTenantIdAndDeletedFalse(identifier, tenantId);
        if (maybe.isPresent()) return maybe;

        return Optional.empty();
    }

    default Optional<User> findByIdentifierForAudits(String rawIdentifier, UUID tenantId) {

        String identifier = rawIdentifier == null ? "" : rawIdentifier.trim();

        try {
            UUID id = UUID.fromString(identifier);
            Optional<User> byId = findByIdAndTenantId(id, tenantId);
            if (byId.isPresent()) return byId;
        } catch (IllegalArgumentException ignored) {}

        Optional<User> maybe =
                findByEmailElementIgnoreCase(identifier.toLowerCase(), tenantId);
        if (maybe.isPresent()) return maybe;

        String phoneNumber = normalizePhone(identifier);

        maybe = findByPhoneNumberElement(phoneNumber, tenantId);
        if (maybe.isPresent()) return maybe;

        maybe = findByUsernameAndTenantId(identifier, tenantId);
        if (maybe.isPresent()) return maybe;

        maybe = findByIdNumberAndTenantId(identifier, tenantId);
        if (maybe.isPresent()) return maybe;

        return Optional.empty();
    }

    default String normalizePhone(String phone) {

        if (phone == null) return null;

        String cleaned = phone.replaceAll("[\\s-]", "");

        if (cleaned.matches("^07\\d{7,8}$")) {
            cleaned = "+254" + cleaned.substring(1);
        } else if (cleaned.matches("^01\\d{7,8}$")) {
            cleaned = "+254" + cleaned.substring(1);
        }

        return cleaned;
    }

    /* ====================== USER DELETE FROM OTHER ENTITIES ====================== */

    @Modifying
    @Transactional
    @Query(value = "DELETE FROM branch_users WHERE user_id = :userId", nativeQuery = true)
    int deleteUserFromBranch(@Param("userId") UUID userId);

    @Modifying
    @Transactional
    @Query(value = "DELETE FROM department_heads WHERE user_id = :userId", nativeQuery = true)
    int deleteUserFromDepartmentHeads(@Param("userId") UUID userId);

    @Modifying
    @Transactional
    @Query(value = "DELETE FROM department_members WHERE user_id = :userId", nativeQuery = true)
    int deleteUserFromDepartmentMembers(@Param("userId") UUID userId);

    default Optional<User> findDeletedByIdentifier(String identifier, UUID tenantId) {

        return findByIdentifierForAudits(identifier, tenantId)
                .filter(User::getDeleted);
    }

    default Optional<User> findByIdentifierIncludingDeleted(String identifier, UUID tenantId) {

        return findByIdentifierForAudits(identifier, tenantId);
    }
}