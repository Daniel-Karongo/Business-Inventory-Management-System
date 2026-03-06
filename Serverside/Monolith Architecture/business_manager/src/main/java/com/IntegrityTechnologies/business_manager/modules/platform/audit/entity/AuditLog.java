package com.IntegrityTechnologies.business_manager.modules.platform.audit.entity;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "audit_log",
        indexes = {
                @Index(name = "idx_audit_tenant", columnList = "tenantId"),
                @Index(name = "idx_audit_entity", columnList = "entityType"),
                @Index(name = "idx_audit_timestamp", columnList = "timestamp")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AuditLog {

    @Id
    @GeneratedValue
    private UUID id;

    private UUID tenantId;

    private UUID userId;

    private String entityType;

    private UUID entityId;

    private String action;

    @Column(columnDefinition = "TEXT")
    private String beforeState;

    @Column(columnDefinition = "TEXT")
    private String afterState;

    private LocalDateTime timestamp;

}