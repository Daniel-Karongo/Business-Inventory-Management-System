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
        indexes = {
                @Index(name = "idx_ub_user", columnList = "user_id"),
                @Index(name = "idx_ub_cred", columnList = "credential_id")
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

    @Column(name = "credential_id", nullable = false, length = 256, unique = true)
    private String credentialId;

    @Column(name = "public_key", nullable = false, columnDefinition = "TEXT")
    private String publicKey;

    private Long signCount;

    private String deviceName;

    private LocalDateTime createdAt;

    @PrePersist
    void onCreate() {
        createdAt = LocalDateTime.now();
        if (signCount == null) signCount = 0L;
    }
}