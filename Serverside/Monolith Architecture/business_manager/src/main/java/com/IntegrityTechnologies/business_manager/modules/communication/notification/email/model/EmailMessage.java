package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Entity
@Table(
        name = "email_messages",
        indexes = {
                @Index(name = "idx_email_status", columnList = "status"),
                @Index(name = "idx_email_sent_at", columnList = "sentAt")
        }
)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class EmailMessage {

    @Id
    @GeneratedValue
    private UUID id;

    @ElementCollection
    @CollectionTable(name = "email_recipients")
    private List<String> recipients;

    private String subject;

    @Lob
    @Column(nullable = false)
    private String body;

    /**
     * QUEUED, SENT, FAILED
     */
    private String status;

    private String error;

    private LocalDateTime createdAt;
    private LocalDateTime sentAt;

    private String createdBy;
}