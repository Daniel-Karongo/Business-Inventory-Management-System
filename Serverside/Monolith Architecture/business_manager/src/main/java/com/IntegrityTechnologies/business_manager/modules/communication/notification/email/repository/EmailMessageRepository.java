package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.repository;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model.EmailMessage;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface EmailMessageRepository extends JpaRepository<EmailMessage, UUID> {
    List<EmailMessage> findByStatus(String status);
}