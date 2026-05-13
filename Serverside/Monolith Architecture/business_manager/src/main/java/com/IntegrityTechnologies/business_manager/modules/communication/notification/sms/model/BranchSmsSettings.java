package com.IntegrityTechnologies.business_manager.modules.communication.notification.sms.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.BranchAwareEntity;
import com.IntegrityTechnologies.business_manager.security.crypto.EncryptedStringConverter;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "branch_sms_settings",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_branch_sms_settings",
                        columnNames = {
                                "tenant_id",
                                "branch_id"
                        }
                )
        },
        indexes = {
                @Index(
                        name = "idx_branch_sms_settings",
                        columnList = "tenant_id, branch_id"
                )
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class BranchSmsSettings extends BranchAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private Boolean enabled = false;

    @Column(nullable = false)
    private String provider;

    @Column(nullable = false)
    private String username;

    @Lob
    @Convert(converter = EncryptedStringConverter.class)
    @Column(nullable = false, columnDefinition = "TEXT")
    private String apiKey;

    @Column(nullable = false)
    private String senderId;

    @Column(nullable = false)
    private String defaultCountryCode;

    @Column(nullable = false)
    private Boolean sandbox = false;

    @Column(nullable = false)
    private Boolean active = true;

    @Column(nullable = false)
    @Builder.Default
    private Boolean credentialsMigrated = false;

    private LocalDateTime credentialsMigratedAt;
}