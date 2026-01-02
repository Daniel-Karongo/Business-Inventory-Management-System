package com.IntegrityTechnologies.business_manager.modules.person.function.auth.model;

import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "password_reset_audit")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PasswordResetAudit {

    @Id
    @GeneratedValue
    private UUID id;

    private UUID userId;
    private String channel;          // EMAIL | SMS | SIMPLE
    private String identifierUsed;   // masked
    private String status;           // REQUESTED | COMPLETED | FAILED
    private String reason;

    private LocalDateTime timestamp;
    private String ipAddress;
}