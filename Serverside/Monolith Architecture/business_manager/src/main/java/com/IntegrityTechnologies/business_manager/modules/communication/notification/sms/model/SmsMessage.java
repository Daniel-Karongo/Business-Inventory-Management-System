package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "sms_messages", indexes = {
        @Index(name = "idx_sms_status", columnList = "status"),
        @Index(name = "idx_sms_phone", columnList = "toPhone")
})
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SmsMessage {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    private String toPhone;   // normalized E.164
    private String fromName;
    private String message;
    private String status;    // PENDING, SENT, FAILED, DELIVERED
    private String providerMessageId; // id from provider
    private LocalDateTime createdAt;
    private LocalDateTime sentAt;
    private String error;
    private String createdBy;
}