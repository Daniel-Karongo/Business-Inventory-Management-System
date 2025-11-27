package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.repository;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsMessage;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface SmsMessageRepository extends JpaRepository<SmsMessage, UUID> {
}
