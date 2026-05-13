package com.IntegrityTechnologies.business_manager.modules.person.user.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.TenantAwareEntity;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.JdbcTypeCode;

import java.sql.Types;
import java.time.LocalDateTime;
import java.util.UUID;

@Data
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Entity
@Table(
        name = "user_image_audits",
        indexes = {

                @Index(name = "idx_user_image_audit_tenant",
                        columnList = "tenant_id"),

                @Index(name = "idx_user_image_audit_user",
                        columnList = "user_id")
        }
)
public class UserImageAudit extends TenantAwareEntity {

    @Id
    @GeneratedValue
    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID id;

    @JdbcTypeCode(Types.BINARY)
    @Column(columnDefinition = "BINARY(16)")
    private UUID userId;

    private String username;

    private String fileName;

    private String filePath;

    private String action;

    private String reason;

    private UUID performedById;

    private String performedByUsername;

    private LocalDateTime timestamp;
}