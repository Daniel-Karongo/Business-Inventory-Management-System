package com.IntegrityTechnologies.business_manager.config.kafka;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "event_outbox",
        indexes = {
                @Index(name = "idx_outbox_tenant_processed", columnList = "tenantId, processed"),
                @Index(name = "idx_outbox_tenant_created", columnList = "tenantId, createdAt"),
                @Index(name = "idx_outbox_processed_created", columnList = "processed, createdAt")
        }
)
@Getter
@Setter
@NoArgsConstructor
public class EventOutbox {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private UUID tenantId;

    @Column(nullable = false)
    private UUID branchId;

    @Column(nullable = false)
    private String eventType;

    @Column(nullable = false, columnDefinition = "TEXT")
    private String payload;

    @Column(nullable = false)
    private boolean processed = false;

    @Column(nullable = false)
    private LocalDateTime createdAt;

    private LocalDateTime processedAt;
}