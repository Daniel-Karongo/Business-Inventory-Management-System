package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.repository;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsMessage;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface SmsMessageRepository extends JpaRepository<SmsMessage, UUID> {

    Optional<SmsMessage> findByProviderMessageId(String providerMessageId);

    List<SmsMessage> findByStatusInAndNextRetryAtBeforeOrNextRetryAtIsNull(
            List<String> statuses,
            LocalDateTime now
    );
}