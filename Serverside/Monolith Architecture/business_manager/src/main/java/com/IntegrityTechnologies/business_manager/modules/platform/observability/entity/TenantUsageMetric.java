package com.IntegrityTechnologies.business_manager.modules.platform.observability.entity;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(name = "tenant_usage_metrics")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TenantUsageMetric {

    @Id
    @GeneratedValue
    private UUID id;

    @Column(nullable = false)
    private UUID tenantId;

    private long requests;

    private long errors;

    private LocalDateTime snapshotTime;
}