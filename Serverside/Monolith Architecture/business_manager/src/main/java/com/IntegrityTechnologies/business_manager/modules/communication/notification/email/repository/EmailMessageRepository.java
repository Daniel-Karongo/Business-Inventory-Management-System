package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.repository;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model.EmailMessage;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface EmailMessageRepository
        extends JpaRepository<EmailMessage, UUID> {

    Optional<EmailMessage>
    findByTenantIdAndBranchIdAndId(
            UUID tenantId,
            UUID branchId,
            UUID id
    );

    List<EmailMessage>
    findByTenantIdAndBranchIdAndStatus(
            UUID tenantId,
            UUID branchId,
            String status
    );

    @Query("""
            SELECT e
            FROM EmailMessage e
            WHERE e.tenantId = :tenantId
            AND e.branchId = :branchId
            AND e.status IN :statuses
            AND e.retryCount < :maxRetries
            AND (
                e.nextRetryAt IS NULL
                OR e.nextRetryAt <= :now
            )
            """)
    List<EmailMessage> findRetryableMessages(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId,
            @Param("statuses") List<String> statuses,
            @Param("maxRetries") int maxRetries,
            @Param("now") LocalDateTime now
    );

    Page<EmailMessage>
    findByTenantIdAndBranchId(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );
}