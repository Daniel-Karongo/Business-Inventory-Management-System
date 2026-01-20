package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.repository;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model.SmsAccount;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface SmsAccountRepository extends JpaRepository<SmsAccount, UUID> {
    Optional<SmsAccount> findByActiveTrue();
}