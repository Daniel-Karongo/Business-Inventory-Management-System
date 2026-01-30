package com.IntegrityTechnologies.business_manager.security.acl.audit;

import jakarta.persistence.*;
import lombok.*;
import java.time.Instant;
import java.util.UUID;

@Entity
@Table(name = "acl_audit_log", indexes = {
        @Index(name = "idx_acl_audit_entity", columnList = "entityType, entityId"),
        @Index(name = "idx_acl_audit_actor", columnList = "actorUserId"),
        @Index(name = "idx_acl_audit_time", columnList = "createdAt")
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AclAuditLog {

    @Id
    @GeneratedValue
    private UUID id;

    @Column(nullable = false)
    private String entityType;   // PERMISSION, ROLE_PERMISSION, CONDITION, ENDPOINT

    @Column(nullable = false)
    private String action;       // CREATE, UPDATE, DELETE, ACTIVATE, DEACTIVATE

    @Column(nullable = false)
    private String entityId;

    @Column(columnDefinition = "json")
    private String beforeState;

    @Column(columnDefinition = "json")
    private String afterState;

    private UUID actorUserId;
    private String actorUsername;
    private String actorRole;

    private String sourceIp;

    private Instant createdAt;
}