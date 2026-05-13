package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.repository;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsMessage;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface SmsMessageRepository
        extends JpaRepository<SmsMessage, UUID> {

    Optional<SmsMessage>
    findByTenantIdAndBranchIdAndId(
            UUID tenantId,
            UUID branchId,
            UUID id
    );

    Optional<SmsMessage>
    findByTenantIdAndBranchIdAndProviderMessageId(
            UUID tenantId,
            UUID branchId,
            String providerMessageId
    );

    @Query("""
            SELECT s
            FROM SmsMessage s
            WHERE s.tenantId = :tenantId
            AND s.branchId = :branchId
            AND s.status IN :statuses
            AND s.retryCount < :maxRetries
            AND (
                s.nextRetryAt IS NULL
                OR s.nextRetryAt <= :now
            )
            """)
    List<SmsMessage> findRetryableMessages(
            @Param("tenantId") UUID tenantId,
            @Param("branchId") UUID branchId,
            @Param("statuses") List<String> statuses,
            @Param("maxRetries") int maxRetries,
            @Param("now") LocalDateTime now
    );

    Page<SmsMessage>
    findByTenantIdAndBranchId(
            UUID tenantId,
            UUID branchId,
            Pageable pageable
    );
}