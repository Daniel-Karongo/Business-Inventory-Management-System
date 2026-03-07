package com.IntegrityTechnologies.business_manager.modules.person.entity.user.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.TenantAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.JdbcTypeCode;

import java.sql.Types;
import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "user_audits",
        indexes = {
                @Index(name = "idx_user_audit_tenant", columnList = "tenant_id"),
                @Index(name = "idx_user_audit_user", columnList = "user_id")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserAudit extends TenantAwareEntity {

    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    private UUID userId;

    private String username;

    private String role;

    private String action;

    private String fieldChanged;

    private String oldValue;

    private String newValue;

    private String reason;

    private UUID performedById;

    private String performedByUsername;

    private LocalDateTime timestamp;
}