package com.IntegrityTechnologies.business_manager.modules.platform.identity.repository;

import com.IntegrityTechnologies.business_manager.modules.platform.identity.entity.PlatformUser;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.Optional;
import java.util.UUID;

public interface PlatformUserRepository
        extends JpaRepository<PlatformUser, UUID> {

    Optional<PlatformUser> findByUsernameAndDeletedFalse(String username);

    boolean existsByUsername(String username);

    @EntityGraph(
            attributePaths = {
                    "emailAddresses",
                    "phoneNumbers"
            }
    )
    Page<PlatformUser> findAllByDeletedFalse(
            Pageable pageable
    );

    @EntityGraph(
            attributePaths = {
                    "emailAddresses",
                    "phoneNumbers"
            }
    )
    @Query("""
            select u
            from PlatformUser u
            where u.id = :id
            and u.deleted = false
            """)
    Optional<PlatformUser> findDetailedById(
            UUID id
    );

    @Query("""
                SELECT DISTINCT u
                FROM PlatformUser u
                WHERE u.deleted = false
                  AND (
                        lower(u.username) = lower(:identifier)
                     OR lower(u.idNumber) = lower(:identifier)
                     OR EXISTS (
                          SELECT 1
                          FROM u.emailAddresses e
                          WHERE lower(e) = lower(:identifier)
                     )
                     OR EXISTS (
                          SELECT 1
                          FROM u.phoneNumbers p
                          WHERE p = :phone
                     )
                  )
            """)
    Optional<PlatformUser> findAuthUser(
            String identifier,
            String phone
    );

    default String normalizePhone(String phone) {

        if (phone == null) return null;

        String cleaned =
                phone.replaceAll("[\\s-]", "");

        if (cleaned.matches("^07\\d{7,8}$")) {
            cleaned = "+254" + cleaned.substring(1);
        } else if (cleaned.matches("^01\\d{7,8}$")) {
            cleaned = "+254" + cleaned.substring(1);
        }

        return cleaned;
    }

    default Optional<PlatformUser> findByIdentifier(
            String rawIdentifier
    ) {

        String identifier =
                rawIdentifier == null
                        ? ""
                        : rawIdentifier.trim();

        return findAuthUser(
                identifier,
                normalizePhone(identifier)
        );
    }
}