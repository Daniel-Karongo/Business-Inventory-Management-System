package com.IntegrityTechnologies.business_manager.config.kafka;

import com.IntegrityTechnologies.business_manager.modules.platform.tenant.model.TenantAwareEntity;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

@Entity
@Table(
        name = "processed_kafka_events",
        uniqueConstraints = @UniqueConstraint(
                columnNames = {"tenant_id","eventId"}
        )
)
@Getter
@Setter
public class ProcessedKafkaEvent extends TenantAwareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false)
    private UUID eventId;
}