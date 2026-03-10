package com.IntegrityTechnologies.business_manager.modules.platform.identity.entity;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.UuidGenerator;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "platform_user_audit")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PlatformUserAudit {

    @Id
    @GeneratedValue
    @UuidGenerator
    private UUID id;

    private UUID userId;

    private String username;

    private String action;

    private String reason;

    private String performedBy;

    private LocalDateTime timestamp;

    @PrePersist
    public void prePersist() {
        timestamp = LocalDateTime.now();
    }

}