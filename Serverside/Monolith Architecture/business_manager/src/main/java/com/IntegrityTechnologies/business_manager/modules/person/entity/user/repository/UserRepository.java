package com.IntegrityTechnologies.business_manager.modules.person.entity.user.repository;

import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.person.entity.user.model.User;
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

    Optional<User> findByUsername(String username);
    Optional<User> findByUsernameAndDeletedFalse(String username);

    Optional<User> findByIdNumberAndDeletedFalse(String idNumber);
    Optional<User> findByIdNumber(String idNumber);

    Optional<User> findByIdAndDeletedFalse(UUID id);
    Optional<User> findById(UUID id);

    List<User> findByDeletedFalse();

    List<User> findByDeletedTrue();

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

    /* ====================== UNIQUE CHECKS (TENANT SAFE) ====================== */

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
        WHERE lower(e) = lower(:email) AND u.deleted = false
    """)
    Optional<User> findByEmailElementIgnoreCaseAndDeletedFalse(@Param("email") String email);
    @Query("""
        SELECT u FROM User u 
        JOIN u.emailAddresses e 
        WHERE lower(e) = lower(:email)
    """)
    Optional<User> findByEmailElementIgnoreCase(@Param("email") String email);

    @Query("""
        SELECT u FROM User u 
        JOIN u.phoneNumbers p 
        WHERE p = :phone AND u.deleted = false
    """)
    Optional<User> findByPhoneNumberElementAndDeletedFalse(@Param("phone") String phone);
    @Query("""
        SELECT u FROM User u 
        JOIN u.phoneNumbers p 
        WHERE p = :phone
    """)
    Optional<User> findByPhoneNumberElement(@Param("phone") String phone);

    /* ====================== ROLE FILTER ====================== */

    @Query("SELECT u FROM User u WHERE u.deleted = false AND u.role = :role")
    List<User> findActiveUsersByRole(@Param("role") Role role);

    @Query("SELECT u FROM User u WHERE u.deleted = true AND u.role = :role")
    List<User> findDeletedUsersByRole(@Param("role") Role role);

    @Query("SELECT u FROM User u WHERE u.role = :role")
    List<User> findAllUsersByRole(@Param("role") Role role);


    /* ====================== IDENTIFIER LOOKUP ====================== */
    @Query("""
        SELECT u FROM User u
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

    /**
     * Full identifier resolver (UUID, email, phone, username, idNumber)
     * Phone is normalized externally before calling this method.
     */
    default Optional<User> findByIdentifier(String rawIdentifier) {

        String identifier = rawIdentifier == null ? "" : rawIdentifier.trim();

        // 1️⃣ Try UUID
        try {
            UUID id = UUID.fromString(identifier);
            Optional<User> byId = findByIdAndDeletedFalse(id);
            if (byId.isPresent()) return byId;
        } catch (IllegalArgumentException ignored) {}

        // 2️⃣ Try email
        Optional<User> maybe = findByEmailElementIgnoreCaseAndDeletedFalse(identifier.toLowerCase());
        if (maybe.isPresent()) return maybe;

        // 3️⃣ Try phone (already normalized before this method is called)
        String phoneNumber = normalizePhone(identifier);
        maybe = findByPhoneNumberElementAndDeletedFalse(phoneNumber);
        if (maybe.isPresent()) return maybe;

        // 4️⃣ Try username
        maybe = findByUsernameAndDeletedFalse(identifier);
        if (maybe.isPresent()) return maybe;

        // 5️⃣ Try ID number
        maybe = findByIdNumberAndDeletedFalse(identifier);
        if (maybe.isPresent()) return maybe;

        return Optional.empty();
    }
    default Optional<User> findByIdentifierForAudits(String rawIdentifier) {

        String identifier = rawIdentifier == null ? "" : rawIdentifier.trim();

        // 1️⃣ Try UUID
        try {
            UUID id = UUID.fromString(identifier);
            Optional<User> byId = findById(id);
            if (byId.isPresent()) return byId;
        } catch (IllegalArgumentException ignored) {}

        // 2️⃣ Try email
        Optional<User> maybe = findByEmailElementIgnoreCase(identifier.toLowerCase());
        if (maybe.isPresent()) return maybe;

        // 3️⃣ Try phone (already normalized before this method is called)
        String phoneNumber = normalizePhone(identifier);
        maybe = findByPhoneNumberElement(phoneNumber);
        if (maybe.isPresent()) return maybe;

        // 4️⃣ Try username
        maybe = findByUsername(identifier);
        if (maybe.isPresent()) return maybe;

        // 5️⃣ Try ID number
        maybe = findByIdNumber(identifier);
        if (maybe.isPresent()) return maybe;

        return Optional.empty();
    }

    default String normalizePhone(String phone) {
        if (phone == null) return null;

        // 1️⃣ Remove spaces and hyphens
        String cleaned = phone.replaceAll("[\\s-]", "");

        // 2️⃣ Convert local formats to international
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

    default Optional<User> findByIdentifierAndTenantId(
            String identifier,
            UUID tenantId
    ) {

        Optional<User> user = findByIdentifier(identifier);

        if (user.isEmpty()) return Optional.empty();

        if (!user.get().getTenantId().equals(tenantId)) {
            return Optional.empty();
        }

        return user;

    }

    Optional<User> findByUsernameAndTenantId(String adminUsername, UUID tenantId);
}