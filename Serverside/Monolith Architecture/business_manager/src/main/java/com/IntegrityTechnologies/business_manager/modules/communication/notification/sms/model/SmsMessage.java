package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "sms_messages",
        indexes = {
                @Index(name = "idx_sms_status", columnList = "status"),
                @Index(name = "idx_sms_phone", columnList = "toPhone"),
                @Index(name = "idx_sms_provider_msg", columnList = "providerMessageId")
        }
)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SmsMessage {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @Column(nullable = false)
    private String toPhone;

    private String fromName;

    @Column(nullable = false, length = 1000)
    private String message;

    /**
     * QUEUED, SENT, RETRY, FAILED, DELIVERED
     */
    @Column(nullable = false)
    private String status;

    @Column(length = 100)
    private String providerMessageId;

    private LocalDateTime createdAt;
    private LocalDateTime sentAt;

    @Column(length = 1000)
    private String error;

    private String createdBy;

    private int retryCount;
    private LocalDateTime nextRetryAt;
}