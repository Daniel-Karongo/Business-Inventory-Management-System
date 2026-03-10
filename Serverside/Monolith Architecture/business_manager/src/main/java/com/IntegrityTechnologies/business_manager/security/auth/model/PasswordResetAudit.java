package com.IntegrityTechnologies.business_manager.security.auth.model;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "password_reset_audit",
        indexes = {
                @Index(name = "idx_password_reset_user", columnList = "user_id"),
                @Index(name = "idx_password_reset_type", columnList = "user_type"),
                @Index(name = "idx_password_reset_tenant", columnList = "tenant_id")
        }
)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PasswordResetAudit {

    @Id
    @GeneratedValue
    private UUID id;

    private UUID userId;

    @Enumerated(EnumType.STRING)
    private UserType userType;

    private UUID tenantId;

    private String channel;          // EMAIL | SMS | SIMPLE
    private String identifierUsed;   // masked
    private String status;           // REQUESTED | COMPLETED | FAILED
    private String reason;

    private LocalDateTime timestamp;
    private String ipAddress;
}