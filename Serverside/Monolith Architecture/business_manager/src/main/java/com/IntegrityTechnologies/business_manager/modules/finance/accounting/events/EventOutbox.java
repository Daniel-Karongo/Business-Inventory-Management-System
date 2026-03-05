package com.IntegrityTechnologies.business_manager.modules.finance.accounting.events;

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
                @Index(name = "idx_outbox_processed", columnList = "processed"),
                @Index(name = "idx_outbox_created", columnList = "createdAt")
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
    private String eventType;

    @Column(nullable = false, columnDefinition = "TEXT")
    private String payload;

    @Column(nullable = false)
    private boolean processed = false;

    @Column(nullable = false)
    private LocalDateTime createdAt;

    private LocalDateTime processedAt;
}