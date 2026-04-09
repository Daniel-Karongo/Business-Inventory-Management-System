package com.IntegrityTechnologies.business_manager.security.device.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.TenantAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "trusted_devices",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_device_branch_device_id",
                        columnNames = {"tenant_id", "branch_id", "device_id"}
                )
        },
        indexes = {
                @Index(name = "idx_td_branch", columnList = "branch_id"),
                @Index(name = "idx_td_device_id", columnList = "device_id")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class TrustedDevice extends TenantAwareEntity {

    @Id
    @GeneratedValue
    private UUID id;

    @Column(name = "branch_id", nullable = true)
    private UUID branchId;

    @Column(name = "device_id", nullable = false, length = 64)
    private String deviceId;

    @Column(name = "device_name")
    private String deviceName;

    @Column(nullable = false)
    @Builder.Default
    private Boolean approved = false;

    private LocalDateTime firstSeenAt;
    private LocalDateTime lastSeenAt;

    @PrePersist
    void onCreate() {
        if (approved == null) approved = false;
        firstSeenAt = LocalDateTime.now();
        lastSeenAt = LocalDateTime.now();
    }
}