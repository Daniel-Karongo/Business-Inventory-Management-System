package com.IntegrityTechnologies.business_manager.modules.person.user.repository;

import com.IntegrityTechnologies.business_manager.modules.person.user.model.User;
import com.IntegrityTechnologies.business_manager.modules.person.user.model.UserImage;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface UserImageRepository extends JpaRepository<UserImage, UUID> {

    /* ================= USER ================= */

    List<UserImage> findByUser(User user);

    Optional<UserImage> findByUserAndFileName(User user, String filename);

    /* ================= TENANT + BRANCH SAFE ================= */

    Page<UserImage> findByTenantId(
            UUID tenantId,
            Pageable pageable
    );

    Page<UserImage> findByTenantIdAndDeleted(
            UUID tenantId,
            Boolean deleted,
            Pageable pageable
    );

    @Query("""
        SELECT ui
        FROM UserImage ui
        JOIN FETCH ui.user
        WHERE ui.tenantId = :tenantId
    """)
    Page<UserImage> findByTenantIdWithUser(UUID tenantId, Pageable pageable);
}