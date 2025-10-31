package com.IntegrityTechnologies.business_manager.modules.user.repository;

import com.IntegrityTechnologies.business_manager.modules.user.model.Role;
import com.IntegrityTechnologies.business_manager.modules.user.model.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface UserRepository extends JpaRepository<User, UUID> {

    Optional<User> findByUsername(String username);
    Optional<User> findByEmailAddress(String emailAddress);
    Optional<User> findByIdNumber(String idNumber);

    default Optional<User> findByIdentifier(String identifier) {
        // Try UUID first
        try {
            UUID id = UUID.fromString(identifier);
            Optional<User> byId = findById(id);
            if (byId.isPresent()) return byId;
        } catch (IllegalArgumentException ignored) {}

        // Fall back to username/email/idNumber
        return findByUsername(identifier)
                .or(() -> findByEmailAddress(identifier))
                .or(() -> findByIdNumber(identifier));
    }

    boolean existsByEmailAddress(String emailAddress);
    boolean existsByUsername(String username);
    boolean existsByIdNumber(String idNumber);

    List<User> findByDeletedFalse();
    List<User> findByDeletedTrue();

    @Query("SELECT u FROM User u WHERE u.deleted = false AND u.role = :role")
    List<User> findActiveUsersByRole(Role role);
}