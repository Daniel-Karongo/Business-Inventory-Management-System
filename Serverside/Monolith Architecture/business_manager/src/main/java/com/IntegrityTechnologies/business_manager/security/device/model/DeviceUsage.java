package com.IntegrityTechnologies.business_manager.security.device.model;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.TenantAwareEntity;
import jakarta.persistence.*;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "device_usage",
        indexes = {
                @Index(name = "idx_du_device", columnList = "device_id"),
                @Index(name = "idx_du_user", columnList = "user_id")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class DeviceUsage extends TenantAwareEntity {

    @Id
    @GeneratedValue
    private UUID id;

    @Column(name = "device_id", nullable = false)
    private UUID deviceId;

    @Column(name = "user_id", nullable = false)
    private UUID userId;

    @Column(name = "last_used_at", nullable = false)
    private LocalDateTime lastUsedAt;
}