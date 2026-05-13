package com.IntegrityTechnologies.business_manager.security.biometric.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.TenantAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "user_biometrics",
        uniqueConstraints = {
                @UniqueConstraint(
                        name="uk_biometric_tenant_credential",
                        columnNames={"tenant_id","credential_id"}
                ),
                @UniqueConstraint(
                        name="uk_biometric_user_device",
                        columnNames={"tenant_id","user_id","device_id"}
                )
        },
        indexes = {
                @Index(name = "idx_ub_user", columnList = "tenant_id, user_id"),
                @Index(name = "idx_ub_cred", columnList = "credential_id"),
                @Index(name = "idx_ub_device", columnList = "device_id")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class UserBiometric extends TenantAwareEntity {

    @Id
    @GeneratedValue
    private UUID id;

    @Column(nullable = false)
    private UUID userId;

    @Column(name = "credential_id", nullable = false, length = 256)
    private String credentialId;

    @Column(name = "public_key", nullable = false, columnDefinition = "TEXT")
    private String publicKey;

    @Column(nullable = false, length = 128)
    private String deviceId;

    private Long signCount;

    private String deviceName;

    @Column(nullable = false)
    @Builder.Default
    private Boolean deleted = false;

    private LocalDateTime deletedAt;
}