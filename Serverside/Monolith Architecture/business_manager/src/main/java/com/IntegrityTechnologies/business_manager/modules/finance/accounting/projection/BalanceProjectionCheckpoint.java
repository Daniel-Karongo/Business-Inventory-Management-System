package com.IntegrityTechnologies.business_manager.modules.finance.accounting.projection;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.UUID;

@Entity
@Table(
        name = "balance_projection_checkpoint",
        uniqueConstraints = @UniqueConstraint(
                columnNames = {"journalId"}
        )
)
@Getter
@Setter
@NoArgsConstructor
public class BalanceProjectionCheckpoint {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(nullable = false, unique = true)
    private UUID journalId;

    @Column(nullable = false)
    private LocalDateTime processedAt;
}