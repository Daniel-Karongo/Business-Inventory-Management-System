package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.service;

import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.dto.EmailRequest;
import com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model.EmailMessage;

import java.util.UUID;

public interface EmailService {

    EmailMessage send(
            UUID branchId,
            EmailRequest request
    );

    EmailMessage get(
            UUID branchId,
            UUID messageId
    );
}