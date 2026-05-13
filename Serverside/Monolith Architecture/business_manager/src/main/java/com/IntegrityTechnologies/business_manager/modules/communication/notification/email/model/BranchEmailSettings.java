package com.IntegrityTechnologies.business_manager.modules.communication.notification.email.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import com.IntegrityTechnologies.business_manager.security.crypto.EncryptedStringConverter;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "branch_email_settings",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_branch_email_settings",
                        columnNames = {
                                "tenant_id",
                                "branch_id"
                        }
                )
        },
        indexes = {
                @Index(
                        name = "idx_branch_email_settings",
                        columnList = "tenant_id, branch_id"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class BranchEmailSettings extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private Boolean enabled = false;

    @Column(nullable = false)
    private String host;

    @Column(nullable = false)
    private Integer port;

    @Column(nullable = false)
    private String username;

    @Column(nullable = false, length = 4000)
    @Convert(converter = EncryptedStringConverter.class)
    private String password;

    @Column(nullable = false)
    private String fromAddress;

    @Column(nullable = false)
    private Boolean tlsEnabled = true;

    @Column(nullable = false)
    private Boolean authEnabled = true;

    @Column(nullable = false)
    private Boolean active = true;

    @Column(nullable = false)
    @Builder.Default
    private Boolean credentialsMigrated = false;

    private LocalDateTime credentialsMigratedAt;
}